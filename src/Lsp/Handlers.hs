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
import Parser.MarkdownWithFrontmatter
import Path
import Project.Link
import Text.RawString.QQ

concatTexts :: [T.Text] -> T.Text
concatTexts texts = TL.toStrict . B.toLazyText $ mconcat (map B.fromText texts)

formatHover :: Path Abs File -> Maybe Metadata -> Either T.Text (Maybe (Int, T.Text)) -> T.Text
formatHover path frontMatter lineContent =
  concatTexts
    [ "In ",
      T.pack (toFilePath path),
      "\n",
      "FrontMatter: ",
      maybe
        ""
        (TE.decodeUtf8With TEE.lenientDecode . encodePretty defConfig)
        frontMatter,
      "\n",
      case lineContent of
        Left err -> "Error:" <> err
        Right Nothing -> ""
        Right (Just (l, txt)) -> "In line " <> T.pack (show l) <> "\n" <> txt,
      "\n"
    ]

textDocumentHoverHandler :: Handlers HandlerM
textDocumentHoverHandler =
  LSP.requestHandler SMethod_TextDocumentHover \request respond -> do
    let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = request
        Position _l _c = pos
        l = _l + 1
        c = _c + 1
        TextDocumentIdentifier uri = _doc
        rsp = Hover (InL ms) (Just range)
        ms = mkMarkdown "Hello world"
        range = Range pos pos
        mpath = uriToFile uri
    let isLink :: MarkdownAstNode -> Bool
        isLink (MarkdownAstNode (Link {}) _ _) = True
        isLink _ = True
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
    mroot <- liftLSP getRootPath
    mfile <- liftLSP $ getVirtualFile (toNormalizedUri uri)
    ret <- runMaybeT $ do
      let parseFile :: Path Abs File -> T.Text -> (Maybe Metadata, Maybe MarkdownAst)
          parseFile path = markdownWithFrontmatter defaultSyntaxSpec (toFilePath path)
      let getAst path file = snd $ parseFile path file

      root <- MaybeT $ return $ mroot >>= parseAbsDir
      origFile <- MaybeT $ return mfile
      origPath <- MaybeT $ return mpath
      origAst <- MaybeT $ return $ getAst origPath (virtualFileText origFile)
      ele <- MaybeT $ return $ nodeAt isLink l c origAst
      link <- MaybeT $ return $ fromLink ele
      (filepath, mtag) <- MaybeT $ return $ parseLink (link ^. linkTarget)

      let rg = do
            sr <- ele ^. sourceRange
            listToMaybe $ sourceRangeToRange sr

      targetPath <- MaybeT $ liftIO $ resolveLinkInFile origPath (T.unpack filepath)
      guard $ root `isProperPrefixOf` targetPath
      targetFile <-
        MaybeT $
          if targetPath == origPath
            then return $ Just origFile
            else liftLSP $ getVirtualFile $ toNormalizedUri $ filePathToUri $ toFilePath targetPath
      let targetText = virtualFileText targetFile
      let (mtargetFrontmatter, mtargetAst) = parseFile targetPath targetText

      let lineContent = case mtag of
            Nothing -> Right Nothing
            Just tag -> case mtargetAst >>= getLineNr tag of
              Nothing -> Left "bookmark not found"
              Just ln -> Right (Just (ln, T.lines targetText !! ln))

      return (rg, formatHover targetPath mtargetFrontmatter lineContent)
    -- respond (Right $ InL rsp)
    respond
      ( case ret of
          Nothing -> Right $ InL rsp
          Just (rg, msg) -> Right $ InL $ Hover (InL (mkMarkdown msg)) rg
      )

initializedHandler :: Handlers HandlerM
initializedHandler =
  LSP.notificationHandler SMethod_Initialized \_ -> return ()

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
      textDocumentDefinitionHandler
    ]
