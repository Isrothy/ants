{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Spec.Lsp.Server (spec) where

import Control.Concurrent
import Control.Exception
import Control.Lens ((^.))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Aeson hiding (Null, defaultOptions)
import Data.Aeson qualified as J
import Data.Default
import Data.Maybe
import Data.Text qualified as T
import GHC.IO.Handle
import Language.LSP.Protocol.Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Language.LSP.Test
import Lsp.Server
import System.Process
import Test.Hspec

withDummyServer :: ((Handle, Handle) -> IO ()) -> IO ()
withDummyServer f = do
  (hinRead, hinWrite) <- createPipe
  (houtRead, houtWrite) <- createPipe

  handlerEnv <- HandlerEnv <$> newEmptyMVar <*> newEmptyMVar
  let definition =
        ServerDefinition
          { doInitialize = \env _req -> pure $ Right env,
            defaultConfig = 1 :: Int,
            configSection = "dummy",
            parseConfig = \_old new -> case fromJSON new of
              J.Success v -> Right v
              J.Error err -> Left $ T.pack err,
            onConfigChange = const $ pure (),
            staticHandlers = \_caps -> handlers',
            interpretHandler = \env ->
              Iso (\m -> runLspT env (runReaderT m handlerEnv)) liftIO,
            options = defaultOptions {optExecuteCommandCommands = Just ["doAnEdit"]}
          }

  bracket
    (forkIO $ void $ runServerWithHandles mempty mempty hinRead houtWrite definition)
    killThread
    (const $ f (hinWrite, houtRead))

data HandlerEnv = HandlerEnv
  { relRegToken :: MVar (RegistrationToken Method_WorkspaceDidChangeWatchedFiles),
    absRegToken :: MVar (RegistrationToken Method_WorkspaceDidChangeWatchedFiles)
  }

handlers' :: Handlers (ReaderT HandlerEnv (LspM Int))
handlers' =
  mconcat
    [ notificationHandler SMethod_Initialized initializedHandler,
      requestHandler SMethod_TextDocumentHover textDocumentHoverHandler,
      requestHandler SMethod_TextDocumentDefinition textDocumentDefinitionHandler
    ]

spec :: Spec
spec = around withDummyServer $ do
  describe "getHover" $
    it "works" $ \(hin, hout) -> runSessionWithHandles hin hout def fullCaps "test/Spec/Lsp/data/dummy" $ do
      doc <- openDoc "dummy.md" "markdown"
      hover <- getHover doc (Position 1 1)
      liftIO $ hover `shouldSatisfy` isJust
