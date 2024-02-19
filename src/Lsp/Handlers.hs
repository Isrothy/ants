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

import Commonmark
import Control.Concurrent.MVar qualified as MVar
import Control.Conditional
import Control.Lens (assign, modifying, use, (^.))
import Control.Monad (forM)
import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (catchE, throwE)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict qualified as State
import Data.ByteString qualified as B
import Data.Maybe
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Text.Utf16.Rope qualified as Rope
import Data.Yaml.Pretty
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP.Types
import Language.LSP.Server
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS
import Language.LSP.VFS qualified as LSP
import Lsp.State
import Lsp.Util
import Model.MarkdownAst
import Model.Metadata
import Network.URI qualified as URI
import Parser.Markdown
import Parser.MarkdownWithFrontmatter
import Path
import Project.Link
import Text.RawString.QQ

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

textDocumentHoverHandler :: Handlers HandlerM
textDocumentHoverHandler =
  LSP.requestHandler SMethod_TextDocumentHover \request respond -> do
    let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = request
        Position _l _c = pos
        l = _l + 1
        c = _c + 1
        TextDocumentIdentifier uri = _doc
        mpath = uriToFile uri
    let isLink :: MarkdownAstNode -> Bool
        isLink (MarkdownAstNode (Link {}) _ _) = True
        isLink _ = False
    let fromLink :: MarkdownAstNode -> Maybe LinkData
        fromLink (MarkdownAstNode (Link ldata) _ _) = Just ldata
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
    mroot <- liftLSP getRootPath
    mfile <- liftLSP $ getVirtualFile (toNormalizedUri uri)
    let linkAtPlace = do
          root <- mroot >>= parseAbsDir
          origFile <- mfile
          origPath <- mpath
          origAst <- getAst origPath (virtualFileText origFile)
          ele <- nodeAt isLink l c origAst
          link <- fromLink ele
          (filepath, mtag) <- parseLink (link ^. linkTarget)
          let rg = do
                sr <- ele ^. sourceRange
                listToMaybe $ sourceRangeToRange sr
          return (rg, root, origPath, origFile, filepath, mtag)

    case linkAtPlace of
      Nothing -> respond $ Right $ InR Null
      Just (rg, root, origPath, origFile, filepath, mtag) -> do
        target <- runMaybeT $ do
          targetPath <- MaybeT $ liftIO $ resolveLinkInFile origPath (T.unpack filepath)
          guard $ root `isProperPrefixOf` targetPath
          targetText <-
            MaybeT $
              if targetPath == origPath
                then return $ Just $ virtualFileText origFile
                else do
                  mvf <-
                    liftLSP $
                      getVirtualFile $
                        toNormalizedUri $
                          filePathToUri $
                            toFilePath targetPath
                  case mvf of
                    Just vf -> return $ Just $ virtualFileText vf
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
              Nothing -> Right $ InL $ Hover (InL (mkMarkdown "target not found")) rg
              Just msg -> Right $ InL $ Hover (InL (mkMarkdown msg)) rg
          )

initializedHandler :: Handlers HandlerM
initializedHandler =
  LSP.notificationHandler SMethod_Initialized \_ -> return ()

workspaceChangeConfigurationHandler :: Handlers HandlerM
workspaceChangeConfigurationHandler =
  LSP.notificationHandler SMethod_WorkspaceDidChangeConfiguration \_ -> return ()

textDocumentChangeHandler :: Handlers HandlerM
textDocumentChangeHandler =
  LSP.notificationHandler SMethod_TextDocumentDidChange \_ -> return ()

cancelationHandler :: Handlers HandlerM
cancelationHandler =
  LSP.notificationHandler SMethod_CancelRequest \_ -> return ()

didOpenTextDocumentNotificationHandler :: Handlers HandlerM
didOpenTextDocumentNotificationHandler =
  LSP.notificationHandler SMethod_TextDocumentDidOpen \_ -> return ()

didSaveTextDocumentNotificationHandler :: Handlers HandlerM
didSaveTextDocumentNotificationHandler =
  LSP.notificationHandler SMethod_TextDocumentDidSave \_ -> return ()

didCloseTextDocumentNotificationHandler :: Handlers HandlerM
didCloseTextDocumentNotificationHandler =
  LSP.notificationHandler SMethod_TextDocumentDidClose \_ -> return ()

textDocumentDefinitionHandler :: Handlers HandlerM
textDocumentDefinitionHandler =
  LSP.requestHandler SMethod_TextDocumentDefinition \request responder -> do
    let TRequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier doc) pos _ _) = request
    responder (Right $ InL $ Definition $ InL $ Location doc $ Range pos pos)

handlers :: Handlers HandlerM
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
