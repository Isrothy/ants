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
import Control.Monad (unless)
import Control.Monad.RWS
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT), exceptToMaybeT, maybeToExceptT)
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
import Model.MarkdownAst
import Model.MarkdownAst.Lenses (HasTarget (target))
import Model.MarkdownAst.Params.HeaderParams
import Model.Metadata
import Parser.Markdown
import Parser.MarkdownWithFrontmatter
import Path
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
      _frontMatter :: Maybe Metadata,
      _targetText :: T.Text,
      _bookmarkResult :: BookmarkResult
    } ->
    LinkResult

data CursourAnalysisResult = NotHover | IsLink LSP.Range (Either LinkException LinkResult)

formatLinkException :: LinkException -> T.Text
formatLinkException TargetNotFound = "**ERROR**: Target Not Found"
formatLinkException OutOfRange = "**WARNING**: Out Of Range"
formatLinkException FileFormatNotSupported = "**INFO**: File Format Not Supported"

formatLinkResult :: LinkResult -> T.Text
formatLinkResult (LinkResult path mfrontMatter targetText bookmarkResult) =
  TL.toStrict . B.toLazyText $ displayFilepath <> displayFrontMatter <> displayContent
  where
    lineCount = 10
    displayFilepath = "In `" <> (B.fromText . T.pack . toFilePath) path <> "` :\n\n"
    displayFrontMatter = case mfrontMatter of
      Nothing -> ""
      Just frontMatter ->
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

parseFile :: MarkdownSyntax -> Path Abs File -> T.Text -> (Maybe Metadata, Maybe MarkdownAst)
parseFile spec path = markdownWithFrontmatter spec (toFilePath path)

linkFromUriFile ::
  MarkdownSyntax ->
  LSP.Uri ->
  LSP.Position ->
  LSP.LspT ServerConfig IO (Maybe (LSP.Range, FilePath, Maybe Bookmark))
linkFromUriFile spec origUri pos = do
  let fromLink :: MdNode -> Maybe T.Text
      fromLink (AstNode (Link ldata) _ _) = Just (ldata ^. target)
      fromLink (AstNode (WikiLink ldata) _ _) = Just (ldata ^. target)
      fromLink _ = Nothing
  let mpath = uriToFile origUri
  mfile <- LSP.getVirtualFile (LSP.toNormalizedUri origUri)
  return $ do
    origFile <- mfile
    origPath <- mpath
    dataPointPos <- VFS.positionToCodePointPosition origFile pos
    let l = dataPointPos ^. VFS.line + 1
        c = dataPointPos ^. VFS.character + 1
    origAst <- snd $ parseFile spec origPath (VFS.virtualFileText origFile)
    ele <- nodeAt isLink l c origAst
    link <- fromLink ele
    (filePath, mtag) <- parseLink link
    rg <- do
      sr <- ele ^. sourceRange
      listToMaybe $ sourceRangeToRange origFile sr
    return (rg, filePath, mtag)

linkedFile ::
  MarkdownSyntax ->
  Path Abs File ->
  FilePath ->
  LSP.LspT ServerConfig IO (Either LinkException (Path Abs File, T.Text, Maybe Metadata, Maybe MarkdownAst))
linkedFile spec origPath linkPath = runExceptT $ do
  targetPath <- maybeToExceptT TargetNotFound $ MaybeT $ liftIO $ resolveLinkInFile origPath linkPath
  targetText <- maybeToExceptT TargetNotFound $ MaybeT $ readLocalOrVFS targetPath
  mroot <- lift LSP.getRootPath
  let root = fromJust $ mroot >>= parseAbsDir
  unless (root `isProperPrefixOf` targetPath) $ throwE OutOfRange
  unless (fileExtension targetPath == Just ".md") $ throwE FileFormatNotSupported
  let (mfrontMatter, mAst) = parseFile spec targetPath targetText
  return (targetPath, targetText, mfrontMatter, mAst)

cursorAnalysis ::
  MarkdownSyntax ->
  LSP.Uri ->
  LSP.Position ->
  LSP.LspT ServerConfig IO CursourAnalysisResult
cursorAnalysis spec origUri pos = do
  link <- linkFromUriFile spec origUri pos
  case link of
    Nothing -> return NotHover
    Just (rg, targetFilePath, mbookmark) -> do
      let path = fromJust (uriToFile origUri)
      ret <- runExceptT $ do
        (targetPath, targetText, mfrontMatter, mtargetAst) <- ExceptT $ linkedFile spec path targetFilePath
        return $
          LinkResult targetPath mfrontMatter targetText $
            case mbookmark of
              Nothing -> NoBookmark $ frontMatterLines targetText + 1
              Just bookmark ->
                maybe
                  BookmarkNotFound
                  BookmarkFound
                  (mtargetAst >>= findBookmarkLine bookmark)
      return $ IsLink rg ret

textDocumentHoverHandler :: LSP.Handlers HandlerM
textDocumentHoverHandler =
  LSP.requestHandler LSP.SMethod_TextDocumentHover \request respond ->
    handleErrorWithDefault respond (LSP.InR LSP.Null) do
      let LSP.TRequestMessage _ _ _ (LSP.HoverParams doc pos _) = request
          LSP.TextDocumentIdentifier origUri = doc
      spec <- use markdownSyntaxSpec
      ret <- liftLSP $ cursorAnalysis spec origUri pos
      case ret of
        NotHover -> respond $ Right $ LSP.InR LSP.Null
        IsLink rg linkResult ->
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
      ret <- liftLSP $ cursorAnalysis spec origUri pos
      case ret of
        NotHover -> respond $ Right $ LSP.InR $ LSP.InR LSP.Null
        IsLink _ linkResult -> case linkResult of
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
        (_, filepath, bookmark) <- MaybeT $ liftLSP $ linkFromUriFile spec origUri pos
        guard $ bookmark == Just ""
        let path = fromJust (uriToFile origUri)
        (targetPath, targetText, mfrontMatter, mtargetAst) <-
          exceptToMaybeT $ ExceptT $ liftLSP $ linkedFile spec path filepath
        targetAst <- MaybeT $ return mtargetAst
        let lines = T.splitLines targetText
        let formatDetail :: Int -> T.Text -> T.Text
            formatDetail lineNr line = fmt ("On line " +| lineNr |+ ": " +| line |+ "")
        let formatDocumentation :: Int -> T.Text
            formatDocumentation lineNr =
              let lineCount = 10
                  displayPath = fmtLn ("In file `" +| toFilePath targetPath |+ "'\n\n")
                  displayFrontMatter = fmtLn ("```yaml\n" +| TE.decodeUtf8With TEE.lenientDecode (encodePretty defConfig mfrontMatter) |+ "\n```\n")
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
      workspaceChangeConfigurationHandler,
      textDocumentChangeHandler,
      cancelationHandler,
      didOpenTextDocumentNotificationHandler,
      didSaveTextDocumentNotificationHandler,
      didCloseTextDocumentNotificationHandler,
      completionHandler
    ]
