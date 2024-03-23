{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lsp.Handlers
  ( handlers,
    textDocumentHoverHandler,
    initializedHandler,
    textDocumentDefinitionHandler,
    liftLSP,
  )
where

import Commonmark (SourceRange (..))
import Commonmark.Types (sourceLine)
import Control.Conditional (guard)
import Control.Lens (use, (.=), (^.))
import Control.Monad (unless, when)
import Control.Monad.Extra (concatMapM)
import Control.Monad.RWS (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Text.LineBreaker qualified as T
import Data.Yaml.Pretty (defConfig, encodePretty)
import Fmt
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Lsp.State
import Lsp.Util
import Model.Config
import Model.Document (Document (..))
import Model.MarkdownAst
import Model.MarkdownAst.Lenses (HasTarget (target))
import Model.MarkdownAst.Params.HeaderParams
import Model.Metadata
import Parser.Markdown
import Parser.MarkdownWithFrontmatter
import Path
import Path.IO
import Project.DocLoader (isHiddenFile, loadAllFromDirectory)
import Project.Link
import Project.ProjectRoot (readConfig)
import Safe (atMay)

handleErrorWithDefault ::
  (Either a1 b -> HandlerM a2) ->
  b ->
  HandlerM a2 ->
  HandlerM a2
handleErrorWithDefault respond _default = flip catchE handler
  where
    handler (Log, _message) = do
      liftLSP $
        LSP.sendNotification
          LSP.SMethod_WindowLogMessage
          (LSP.LogMessageParams LSP.MessageType_Log _message)
      respond (Right _default)
    handler (severity_, _message) = do
      let _xtype = case severity_ of
            Error -> LSP.MessageType_Error
            Warning -> LSP.MessageType_Warning
            Info -> LSP.MessageType_Info
      liftLSP $
        LSP.sendNotification
          LSP.SMethod_WindowShowMessage
          (LSP.ShowMessageParams _xtype _message)
      respond (Right _default)

data BookmarkResult = NoBookmark Int | BookmarkNotFound | BookmarkFound Int

data LinkException
  = TargetNotFound
  | OutOfRange
  | FileFormatNotSupported

data LinkResult where
  LinkResult ::
    { _path :: Path Abs File,
      _frontMatter :: Metadata,
      _targetText :: T.Text,
      _bookmarkResult :: BookmarkResult
    } ->
    LinkResult

data DefinitionAnalysisResult where
  IsLink ::
    LSP.Range ->
    (Either LinkException LinkResult) ->
    DefinitionAnalysisResult

formatLinkException :: LinkException -> T.Text
formatLinkException TargetNotFound = "**ERROR**: Target Not Found"
formatLinkException OutOfRange = "**WARNING**: Out Of Range"
formatLinkException FileFormatNotSupported = "**INFO**: File Format Not Supported"

formatLinkResult :: LinkResult -> T.Text
formatLinkResult (LinkResult path frontMatter targetText bookmarkResult) =
  TL.toStrict . B.toLazyText $ displayFilepath <> displayFrontMatter <> displayContent
  where
    lineCount = 10
    displayFilepath = "In `" <> (B.fromText . T.pack . toFilePath) path <> "` :\n\n"
    displayFrontMatter =
      "```yaml\n"
        <> B.fromText (TE.decodeUtf8With TEE.lenientDecode (encodePretty defConfig frontMatter))
        <> "```\n\n"
    displayContent = case bookmarkResult of
      NoBookmark ln ->
        B.fromText
          (T.joinLines $ take lineCount $ drop (ln - 1) $ T.splitLines targetText)
      BookmarkNotFound -> "**ERROR**: Bookmark Not Found \n"
      BookmarkFound ln ->
        "Line "
          <> (B.fromText . T.pack . show) ln
          <> ":\n\n"
          <> B.fromText (T.joinLines $ take lineCount $ drop (ln - 1) $ T.splitLines targetText)

linkFromAst :: (Integral i) => MarkdownAst -> (i, i) -> Maybe (SourceRange, FilePath, Maybe Bookmark)
linkFromAst ast (l, c) = do
  let fromLink :: MdNode -> Maybe T.Text
      fromLink (AstNode (Link ldata) _ _) = Just (ldata ^. target)
      fromLink (AstNode (WikiLink ldata) _ _) = Just (ldata ^. target)
      fromLink _ = Nothing
  ele <- nodeAt isLink l c ast
  link <- fromLink ele
  (filePath, mtag) <- parseLink link
  sr <- ele ^. sourceRange
  return (sr, filePath, mtag)

linkedFile ::
  MarkdownSyntax ->
  Path Abs File ->
  FilePath ->
  LSP.LspT ServerConfig IO (Either LinkException Document)
linkedFile spec origPath linkPath = runExceptT $ do
  targetPath <- maybeToExceptT TargetNotFound $ MaybeT $ liftIO $ resolveLinkInFile origPath linkPath
  root <- maybeToExceptT TargetNotFound $ MaybeT $ fmap (>>= parseAbsDir) LSP.getRootPath
  relPath <- maybeToExceptT OutOfRange $ MaybeT $ return $ stripProperPrefix root targetPath
  when (isHiddenFile relPath) $ throwE OutOfRange
  unless (fileExtension targetPath == Just ".md") $ throwE FileFormatNotSupported
  maybeToExceptT TargetNotFound $ MaybeT $ loadLocalOrVirtualDocument spec root relPath

definitionAnalysis ::
  MarkdownSyntax ->
  LSP.Uri ->
  LSP.Position ->
  LSP.LspT ServerConfig IO (Maybe DefinitionAnalysisResult)
definitionAnalysis spec origUri pos = do
  runMaybeT $ do
    origFile <- MaybeT $ LSP.getVirtualFile (LSP.toNormalizedUri origUri)
    origPath <- MaybeT $ return $ uriToFile origUri
    dataPointPos <- MaybeT $ return $ VFS.positionToCodePointPosition origFile pos
    let l = dataPointPos ^. VFS.line + 1
        c = dataPointPos ^. VFS.character + 1
    origAst <- MaybeT $ return $ snd $ markdownWithFrontmatter spec (toFilePath origPath) $ VFS.virtualFileText origFile
    (sr, targetFilePath, mbookmark) <- MaybeT $ return $ linkFromAst origAst (l, c)
    rg <- MaybeT $ return $ listToMaybe $ sourceRangeToRange origFile sr
    ret <- lift $ runExceptT $ do
      doc <- ExceptT $ linkedFile spec origPath targetFilePath
      return $
        LinkResult (absPath doc) (metadata doc) (text doc) $
          case mbookmark of
            Nothing -> NoBookmark $ frontMatterLines (text doc) + 1
            Just bookmark ->
              maybe
                BookmarkNotFound
                BookmarkFound
                (ast doc >>= findBookmarkLine bookmark)
    return $ IsLink rg ret

textDocumentReferencesHandler :: LSP.Handlers HandlerM
textDocumentReferencesHandler =
  LSP.requestHandler LSP.SMethod_TextDocumentReferences \request respond -> do
    handleErrorWithDefault respond (LSP.InR LSP.Null) do
      let LSP.TRequestMessage _ _ _ (LSP.ReferenceParams doc pos _ _ _) = request
          LSP.TextDocumentIdentifier origUri = doc
      spec <- use markdownSyntaxSpec
      items <- runMaybeT $ do
        root <- MaybeT $ liftLSP $ fmap (>>= parseAbsDir) LSP.getRootPath
        origFile <- MaybeT $ liftLSP $ LSP.getVirtualFile (LSP.toNormalizedUri origUri)
        origPath <- MaybeT $ return $ uriToFile origUri
        dataPointPos <- MaybeT $ return $ VFS.positionToCodePointPosition origFile pos
        let l = dataPointPos ^. VFS.line + 1
            c = dataPointPos ^. VFS.character + 1
        origAst <- MaybeT $ return $ snd $ markdownWithFrontmatter spec (toFilePath origPath) $ VFS.virtualFileText origFile
        header <- MaybeT $ return $ headerAt l c origAst
        id <- MaybeT $ return $ lookup "id" (header ^. attributes)
        docs <- liftIO $ loadAllFromDirectory spec root
        let referenceFromLocalDoc doc = do
              let path = absPath doc
              let uri = LSP.filePathToUri $ toFilePath path
              mvfile <- liftLSP $ LSP.getVirtualFile $ LSP.toNormalizedUri uri
              let mAst = case mvfile of
                    Just vfile -> snd $ markdownWithFrontmatter spec (toFilePath path) (VFS.virtualFileText vfile)
                    Nothing -> ast doc
              let toRange sr = head case mvfile of
                    Just vfile -> sourceRangeToRange vfile sr
                    Nothing -> sourceRangeToRangeT (text doc) sr
              case mAst of
                Nothing -> return []
                Just ast ->
                  let links = linksTo root path ast (toFilePath origPath, Just id)
                      refs = fmap (map (LSP.Location uri . toRange)) links
                   in liftLSP $ liftIO refs
        MaybeT $ Just <$> concatMapM referenceFromLocalDoc docs
      respond $ Right $ LSP.InL $ fromMaybe [] items

textDocumentHoverHandler :: LSP.Handlers HandlerM
textDocumentHoverHandler =
  LSP.requestHandler LSP.SMethod_TextDocumentHover \request respond ->
    handleErrorWithDefault respond (LSP.InR LSP.Null) do
      let LSP.TRequestMessage _ _ _ (LSP.HoverParams doc pos _) = request
          LSP.TextDocumentIdentifier origUri = doc
      spec <- use markdownSyntaxSpec
      ret <- liftLSP $ definitionAnalysis spec origUri pos
      case ret of
        Nothing -> respond $ Right $ LSP.InR LSP.Null
        Just (IsLink rg linkResult) ->
          respond $
            Right $
              LSP.InL $
                LSP.Hover
                  (LSP.InL $ LSP.mkMarkdown $ either formatLinkException formatLinkResult linkResult)
                  (Just rg)

textDocumentDefinitionHandler :: LSP.Handlers HandlerM
textDocumentDefinitionHandler =
  LSP.requestHandler LSP.SMethod_TextDocumentDefinition \request respond -> do
    handleErrorWithDefault respond (LSP.InR $ LSP.InR LSP.Null) do
      let LSP.TRequestMessage _ _ _ (LSP.DefinitionParams doc pos _ _) = request
          LSP.TextDocumentIdentifier origUri = doc
      spec <- use markdownSyntaxSpec
      ret <- liftLSP $ definitionAnalysis spec origUri pos
      case ret of
        Nothing -> respond $ Right $ LSP.InR $ LSP.InR LSP.Null
        Just (IsLink _ linkResult) -> case linkResult of
          Right (LinkResult path _ _ bookmarkResult) ->
            let uri = LSP.filePathToUri (toFilePath path)
                rg = case bookmarkResult of
                  NoBookmark ln -> LSP.mkRange (fromIntegral ln - 1) 0 (fromIntegral ln) 0
                  BookmarkFound ln -> LSP.mkRange (fromIntegral ln - 1) 0 (fromIntegral ln) 0
                  BookmarkNotFound -> LSP.mkRange 0 0 1 0
             in respond $ Right $ LSP.InL $ LSP.Definition $ LSP.InL $ LSP.Location uri rg
          _ -> respond $ Right $ LSP.InR $ LSP.InR LSP.Null

initializedHandler :: LSP.Handlers HandlerM
initializedHandler =
  LSP.notificationHandler LSP.SMethod_Initialized \_ -> do
    mconfig <- runMaybeT $ do
      mroot <- MaybeT $ liftLSP LSP.getRootPath
      root <- MaybeT $ return $ parseAbsDir mroot
      MaybeT $ liftIO $ readConfig root
    markdownSyntaxSpec .= getSyntaxSpec (fromMaybe def mconfig)
    return ()

completionHandler :: LSP.Handlers HandlerM
completionHandler =
  LSP.requestHandler LSP.SMethod_TextDocumentCompletion \request respond -> do
    handleErrorWithDefault respond (LSP.InR $ LSP.InL (LSP.CompletionList True Nothing [])) do
      let LSP.TRequestMessage _ _ _ (LSP.CompletionParams doc pos _ _ _) = request
          LSP.TextDocumentIdentifier origUri = doc
      spec <- use markdownSyntaxSpec
      items <- runMaybeT $ do
        origFile <- MaybeT $ liftLSP $ LSP.getVirtualFile (LSP.toNormalizedUri origUri)
        origPath <- MaybeT $ return $ uriToFile origUri
        dataPointPos <- MaybeT $ return $ VFS.positionToCodePointPosition origFile pos
        let l = dataPointPos ^. VFS.line + 1
            c = dataPointPos ^. VFS.character + 1
        origAst <- MaybeT $ return $ snd $ markdownWithFrontmatter spec (toFilePath origPath) $ VFS.virtualFileText origFile
        (_, filepath, bookmark) <- MaybeT $ return $ linkFromAst origAst (l, c)
        guard $ bookmark == Just ""
        let path = fromJust (uriToFile origUri)
        doc <- exceptToMaybeT $ ExceptT $ liftLSP $ linkedFile spec path filepath
        targetAst <- MaybeT $ return $ ast doc
        let lines = T.splitLines $ text doc
        let formatDetail :: Int -> T.Text -> T.Text
            formatDetail lineNr line = fmt ("On line " +| lineNr |+ ": " +| line |+ "")
        let formatDocumentation :: Int -> T.Text
            formatDocumentation lineNr =
              let lineCount = 10
                  displayPath = fmtLn ("In file `" +| toFilePath (absPath doc) |+ "'\n\n")
                  displayFrontMatter = fmtLn ("```yaml\n" +| TE.decodeUtf8With TEE.lenientDecode (encodePretty defConfig (metadata doc)) |+ "\n```\n")
                  displayContent = fmtLn ("Line: " +| lineNr |+ "\n\n" +| T.joinLines (take lineCount $ drop (lineNr - 1) lines) |+ "")
               in displayPath <> displayFrontMatter <> displayContent
        let headerToItem :: MdNode -> Maybe LSP.CompletionItem
            headerToItem (AstNode (Header (HeaderParams _ _)) msr attrs) = do
              label <- lookup "id" attrs
              sr <- msr
              lineNr <- case unSourceRange sr of
                (begin, _) : _ -> Just $ sourceLine begin
                _ -> Nothing
              line <- fmap fst (atMay lines (lineNr - 1))
              return
                LSP.CompletionItem
                  { _label = label,
                    _labelDetails = Nothing,
                    _kind = Just LSP.CompletionItemKind_Reference,
                    _tags = Nothing,
                    _detail = Just $ formatDetail lineNr line,
                    _documentation = Just $ LSP.InR $ LSP.mkMarkdown $ formatDocumentation lineNr,
                    _deprecated = Nothing,
                    _preselect = Nothing,
                    _sortText = Nothing,
                    _filterText = Nothing,
                    _insertText = Nothing,
                    _insertTextFormat = Nothing,
                    _insertTextMode = Nothing,
                    _textEdit = Nothing,
                    _textEditText = Nothing,
                    _additionalTextEdits = Nothing,
                    _commitCharacters = Nothing,
                    _command = Nothing,
                    _data_ = Nothing
                  }
            headerToItem _ = Nothing
        return $ allNodes' headerToItem targetAst
      respond $ Right $ LSP.InR $ LSP.InL $ LSP.CompletionList False Nothing $ fromMaybe [] items

workspaceChangeConfigurationHandler :: LSP.Handlers HandlerM
workspaceChangeConfigurationHandler =
  LSP.notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration \_ -> return ()

textDocumentChangeHandler :: LSP.Handlers HandlerM
textDocumentChangeHandler =
  LSP.notificationHandler LSP.SMethod_TextDocumentDidChange \_ -> return ()

cancelationHandler :: LSP.Handlers HandlerM
cancelationHandler =
  LSP.notificationHandler LSP.SMethod_CancelRequest \_ -> return ()

didOpenTextDocumentNotificationHandler :: LSP.Handlers HandlerM
didOpenTextDocumentNotificationHandler =
  LSP.notificationHandler LSP.SMethod_TextDocumentDidOpen \_ -> return ()

didSaveTextDocumentNotificationHandler :: LSP.Handlers HandlerM
didSaveTextDocumentNotificationHandler =
  LSP.notificationHandler LSP.SMethod_TextDocumentDidSave \_ -> return ()

didCloseTextDocumentNotificationHandler :: LSP.Handlers HandlerM
didCloseTextDocumentNotificationHandler =
  LSP.notificationHandler LSP.SMethod_TextDocumentDidClose \_ -> return ()

handlers :: LSP.Handlers HandlerM
handlers =
  mconcat
    [ initializedHandler,
      textDocumentHoverHandler,
      textDocumentDefinitionHandler,
      textDocumentReferencesHandler,
      workspaceChangeConfigurationHandler,
      textDocumentChangeHandler,
      cancelationHandler,
      didOpenTextDocumentNotificationHandler,
      didSaveTextDocumentNotificationHandler,
      didCloseTextDocumentNotificationHandler,
      completionHandler
    ]
