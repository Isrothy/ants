{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Control.Conditional (guard)
import Control.Lens (modifying, use, (.=), (^.))
import Control.Monad.RWS
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Default
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Yaml.Pretty (defConfig, encodePretty)
import Language.LSP.Protocol.Lens qualified as LSP
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Lsp.State
import Lsp.Util
import Model.Config
import Model.MarkdownAst
import Model.Metadata
import Parser.Markdown
import Parser.MarkdownWithFrontmatter
import Path
import Project.Link
import Project.ProjectRoot (readConfig)
import Text.RawString.QQ

handleErrorWithDefault ::
  (Either a1 b -> HandlerM a2) ->
  b ->
  HandlerM a2 ->
  HandlerM a2
handleErrorWithDefault respond _default = flip catchE handler
  where
    handler (Log, _message) = do
      liftLSP $ LSP.sendNotification LSP.SMethod_WindowLogMessage (LSP.LogMessageParams LSP.MessageType_Log _message)
      respond (Right _default)
    handler (severity_, _message) = do
      let _xtype = case severity_ of
            Error -> LSP.MessageType_Error
            Warning -> LSP.MessageType_Warning
            Info -> LSP.MessageType_Info
      liftLSP $ LSP.sendNotification LSP.SMethod_WindowShowMessage (LSP.ShowMessageParams _xtype _message)
      respond (Right _default)

data BookmarkResult = NoBookmark Int | BookmarkNotFound | BookmarkFound Int

data LinkResult
  = TargetNotFound
  | TargetFound
      { _path :: Path Abs File,
        _frontMatter :: Maybe Metadata,
        _targetText :: T.Text,
        _bookmarkResult :: BookmarkResult
      }

data CursourAnalysisResult = NotHover | IsLink LSP.Range LinkResult

formatLinkResult :: LinkResult -> T.Text
formatLinkResult TargetNotFound = "**ERROR**: target not found"
formatLinkResult (TargetFound path mfrontMatter targetText bookmarkResult) =
  TL.toStrict . B.toLazyText $ displayFilepath <> displayFrontMatter <> displayContent
  where
    lineCount = 10
    displayFilepath = "In `" <> (B.fromText . T.pack . toFilePath) path <> "` :\n\n"
    displayFrontMatter = case mfrontMatter of
      Nothing -> ""
      Just frontMatter -> "```yaml\n" <> B.fromText (TE.decodeUtf8With TEE.lenientDecode (encodePretty defConfig frontMatter)) <> "```\n\n"
    displayContent = case bookmarkResult of
      NoBookmark ln -> B.fromText (T.unlines $ take lineCount $ drop (ln - 1) $ T.lines targetText)
      BookmarkNotFound -> "**ERROR**: Bookmark Not Found \n"
      BookmarkFound ln ->
        "Line "
          <> (B.fromText . T.pack . show) ln
          <> ":\n\n"
          <> B.fromText (T.unlines $ take lineCount $ drop (ln - 1) $ T.lines targetText)

parseFile :: MarkdownSyntax -> Path Abs File -> T.Text -> (Maybe Metadata, Maybe MarkdownAst)
parseFile spec path = markdownWithFrontmatter spec (toFilePath path)

linkFromUriFile ::
  MarkdownSyntax ->
  LSP.Uri ->
  LSP.Position ->
  LSP.LspT ServerConfig IO (Maybe (LSP.Range, FilePath, Maybe Bookmark))
linkFromUriFile spec origUri pos = do
  let fromLink :: MarkdownAstNode -> Maybe T.Text
      fromLink (MarkdownAstNode (Link ldata) _ _) = Just (ldata ^. linkTarget)
      fromLink (MarkdownAstNode (WikiLink ldata) _ _) = Just (ldata ^. wikiLinkTarget)
      fromLink _ = Nothing
  let LSP.Position _l _c = pos
      l = _l + 1
      c = _c + 1
  let mpath = uriToFile origUri
  mfile <- LSP.getVirtualFile (LSP.toNormalizedUri origUri)
  runMaybeT $ do
    origFile <- MaybeT $ return mfile
    origPath <- MaybeT $ return mpath
    origAst <- MaybeT $ return $ snd $ parseFile spec origPath (VFS.virtualFileText origFile)
    ele <- MaybeT $ return $ nodeAt isLink l c origAst
    link <- MaybeT $ return $ fromLink ele
    (filePath, mtag) <- MaybeT $ return $ parseLink link
    rg <- MaybeT $ return $ do
      sr <- ele ^. sourceRange
      listToMaybe $ sourceRangeToRange sr
    return (rg, T.unpack filePath, mtag)

linkedFile ::
  MarkdownSyntax ->
  Path Abs Dir ->
  Path Abs File ->
  FilePath ->
  LSP.LspT ServerConfig IO (Maybe (Path Abs File, T.Text, Maybe Metadata, Maybe MarkdownAst))
linkedFile spec root origPath linkPath = runMaybeT $ do
  targetPath <- MaybeT $ liftIO $ resolveLinkInFile origPath linkPath
  guard $ root `isProperPrefixOf` targetPath
  targetText <- MaybeT $ readLocalOrVFS targetPath
  let (mfrontMatter, mAst) = parseFile spec targetPath targetText
  return (targetPath, targetText, mfrontMatter, mAst)

cursorAnalysis ::
  MarkdownSyntax ->
  LSP.Uri ->
  LSP.Position ->
  LSP.LspT ServerConfig IO CursourAnalysisResult
cursorAnalysis spec origUri pos = do
  mroot <- LSP.getRootPath
  link <- linkFromUriFile spec origUri pos
  case link of
    Nothing -> return NotHover
    Just (rg, targetFilePath, mbookmark) -> do
      let path = fromJust (uriToFile origUri)
      tar <- runMaybeT $ do
        root <- MaybeT $ return (mroot >>= parseAbsDir)
        MaybeT $ linkedFile spec root path targetFilePath
      case tar of
        Nothing -> return $ IsLink rg TargetNotFound
        Just (targetPath, targetText, mfrontMatter, mtargetAst) ->
          case mbookmark of
            Nothing -> return $ IsLink rg $ TargetFound targetPath mfrontMatter targetText $ NoBookmark $ frontMatterLines targetText + 1
            Just bookmark -> case mtargetAst >>= findBookmarkLine bookmark of
              Nothing -> return $ IsLink rg $ TargetFound targetPath mfrontMatter targetText BookmarkNotFound
              Just ln -> return $ IsLink rg $ TargetFound targetPath mfrontMatter targetText $ BookmarkFound ln

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
        IsLink rg linkResult -> respond $ Right $ LSP.InL $ LSP.Hover (LSP.InL $ LSP.mkMarkdown $ formatLinkResult linkResult) (Just rg)

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
          TargetNotFound -> respond $ Right $ LSP.InR $ LSP.InR LSP.Null
          TargetFound path _ _ bookmarkResult ->
            let uri = LSP.filePathToUri (toFilePath path)
             in case bookmarkResult of
                  NoBookmark ln -> respond $ Right $ LSP.InL $ LSP.Definition $ LSP.InL $ LSP.Location uri $ LSP.mkRange (fromIntegral ln - 1) 0 (fromIntegral ln) 0
                  BookmarkNotFound -> respond $ Right $ LSP.InL $ LSP.Definition $ LSP.InL $ LSP.Location uri $ LSP.mkRange 0 0 1 0
                  BookmarkFound ln -> respond $ Right $ LSP.InL $ LSP.Definition $ LSP.InL $ LSP.Location uri $ LSP.mkRange (fromIntegral ln - 1) 0 (fromIntegral ln) 0

initializedHandler :: LSP.Handlers HandlerM
initializedHandler =
  LSP.notificationHandler LSP.SMethod_Initialized \_ -> do
    mconfig <- runMaybeT $ do
      mroot <- MaybeT $ liftLSP LSP.getRootPath
      root <- MaybeT $ return $ parseAbsDir mroot
      MaybeT $ liftIO $ readConfig root
    markdownSyntaxSpec .= getSyntaxSpec (fromMaybe def mconfig)
    return ()

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
      didCloseTextDocumentNotificationHandler
    ]
