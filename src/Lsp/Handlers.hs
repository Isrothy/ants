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

import Control.Applicative ((<|>))
import Control.Concurrent.MVar qualified as MVar
import Control.Conditional
import Control.Lens (assign, modifying, use, (^.))
import Control.Monad (forM, guard)
import Control.Monad.IO.Class
import Control.Monad.RWS
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (catchE, throwE)
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict qualified as State
import Data.Maybe
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Utf16.Rope qualified as Rope
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP.Types
import Language.LSP.Server
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as LSP
import Lsp.State
import Lsp.Util
import Network.URI qualified as URI
import Path
import Project.ProjectRoot
import System.IO (stderr)

liftLSP :: LspT ServerConfig IO a -> HandlerM a
liftLSP m = lift (lift m)

readUri :: Uri -> HandlerM Text.Text
readUri uri_ = do
  mVirtualFile <- liftLSP (LSP.getVirtualFile (LSP.Types.toNormalizedUri uri_))
  case mVirtualFile of
    Just (LSP.VirtualFile _ _ rope) -> return (Rope.toText rope)
    Nothing -> Except.throwE (Error, "Could not find " <> Text.pack (show uri_) <> " in VFS.")

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
    mroot <- liftLSP getRootPath
    mfile <- liftLSP $ getVirtualFile (toNormalizedUri uri)
    msg <- runMaybeT $ do
      file <- MaybeT $ return mfile

      path <- MaybeT $ return mpath
      root <- MaybeT $ return $ mroot >>= parseAbsDir
      rel <- MaybeT $ return $ stripProperPrefix root path
      -- file <- getVirtualFile
      return Nothing
    respond (Right $ InL rsp)

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
