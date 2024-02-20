{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
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

import Commonmark (SourceRange (unSourceRange), sourceLine)
import Control.Conditional (guard)
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (catchE)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Yaml.Pretty (defConfig, encodePretty)
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Lsp.State
import Lsp.Util
import Model.MarkdownAst
import Model.Metadata
import Parser.Markdown
import Parser.MarkdownWithFrontmatter
import Path
import Project.Link

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

formatHover :: Path Abs File -> Maybe Metadata -> Either T.Text (Maybe Int, [T.Text]) -> T.Text
formatHover path mfrontMatter preview =
  TL.toStrict . B.toLazyText $ displayFilepath <> displayFrontMatter <> displayContent
  where
    lineCount = 10
    displayFilepath = "In `" <> (B.fromText . T.pack . toFilePath) path <> "` :\n\n"
    displayFrontMatter = case mfrontMatter of
      Nothing -> ""
      Just frontMatter -> "```yaml\n" <> B.fromText (TE.decodeUtf8With TEE.lenientDecode (encodePretty defConfig frontMatter)) <> "```\n\n"
    displayContent = case preview of
      Left err -> "**ERROR**: " <> B.fromText err <> "\n\n"
      Right (Nothing, text) -> B.fromText (T.unlines (take lineCount text))
      Right (Just l, txt) ->
        "Line "
          <> (B.fromText . T.pack . show) l
          <> ":\n\n"
          <> B.fromText (T.unlines (take lineCount txt))

textDocumentHoverHandler :: LSP.Handlers HandlerM
textDocumentHoverHandler =
  LSP.requestHandler LSP.SMethod_TextDocumentHover \request respond -> handleErrorWithDefault respond (LSP.InR LSP.Null) do
    let LSP.TRequestMessage _ _ _ (LSP.HoverParams doc pos _) = request
        LSP.Position _l _c = pos
        l = _l + 1
        c = _c + 1
        LSP.TextDocumentIdentifier uri = doc
        mpath = uriToFile uri
    let isLink :: MarkdownAstNode -> Bool
        isLink (MarkdownAstNode (Link {}) _ _) = True
        isLink (MarkdownAstNode (WikiLink {}) _ _) = True
        isLink _ = False
    let fromLink :: MarkdownAstNode -> Maybe T.Text
        fromLink (MarkdownAstNode (Link ldata) _ _) = Just (ldata ^. linkTarget)
        fromLink (MarkdownAstNode (WikiLink ldata) _ _) = Just (ldata ^. wikiLinkTarget)
        fromLink _ = Nothing
    let getLineNr :: T.Text -> MarkdownAst -> Maybe Int
        getLineNr tag ast = do
          ele <- findHeaderWithId tag ast
          sr <- ele ^. sourceRange
          case unSourceRange sr of
            (begin, _) : _ -> Just $ sourceLine begin
            _ -> Nothing
    let parseFile :: Path Abs File -> T.Text -> (Maybe Metadata, Maybe MarkdownAst)
        parseFile path = markdownWithFrontmatter allSpecExtensions (toFilePath path)
    let getAst path file = snd $ parseFile path file
    mroot <- liftLSP LSP.getRootPath
    mfile <- liftLSP $ LSP.getVirtualFile (LSP.toNormalizedUri uri)
    let linkAtPlace = do
          root <- mroot >>= parseAbsDir
          origFile <- mfile
          origPath <- mpath
          origAst <- getAst origPath (VFS.virtualFileText origFile)
          ele <- nodeAt isLink l c origAst
          link <- fromLink ele
          (filepath, mtag) <- parseLink link
          let rg = do
                sr <- ele ^. sourceRange
                listToMaybe $ sourceRangeToRange sr
          return (rg, root, origPath, origFile, filepath, mtag)

    case linkAtPlace of
      Nothing -> respond $ Right $ LSP.InR LSP.Null
      Just (rg, root, origPath, origFile, filepath, mtag) -> do
        target <- runMaybeT $ do
          targetPath <- MaybeT $ liftIO $ resolveLinkInFile origPath (T.unpack filepath)
          guard $ root `isProperPrefixOf` targetPath
          targetText <-
            MaybeT $
              if targetPath == origPath
                then return $ Just $ VFS.virtualFileText origFile
                else do
                  mvf <- liftLSP $ LSP.getVirtualFile $ LSP.toNormalizedUri $ LSP.filePathToUri $ toFilePath targetPath
                  case mvf of
                    Just vf -> return $ Just $ VFS.virtualFileText vf
                    Nothing -> liftIO $ readFileSafe $ toFilePath targetPath
          let (mtargetFrontmatter, mtargetAst) = parseFile targetPath targetText

          let preview = case mtag of
                Nothing -> Right (Nothing, drop (frontMatterLines targetText) (T.lines targetText))
                Just tag -> case mtargetAst >>= getLineNr tag of
                  Nothing -> Left "bookmark not found"
                  Just ln -> Right (Just ln, drop (ln - 1) (T.lines targetText))
          return $ formatHover targetPath mtargetFrontmatter preview
        respond
          ( case target of
              Nothing -> Right $ LSP.InL $ LSP.Hover (LSP.InL (LSP.mkMarkdown "target not found")) rg
              Just msg -> Right $ LSP.InL $ LSP.Hover (LSP.InL (LSP.mkMarkdown msg)) rg
          )

initializedHandler :: LSP.Handlers HandlerM
initializedHandler =
  LSP.notificationHandler LSP.SMethod_Initialized \_ -> return ()

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

textDocumentDefinitionHandler :: LSP.Handlers HandlerM
textDocumentDefinitionHandler =
  LSP.requestHandler LSP.SMethod_TextDocumentDefinition \request responder -> do
    let LSP.TRequestMessage _ _ _ (LSP.DefinitionParams (LSP.TextDocumentIdentifier doc) pos _ _) = request
    responder (Right $ LSP.InL $ LSP.Definition $ LSP.InL $ LSP.Location doc $ LSP.Range pos pos)

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
