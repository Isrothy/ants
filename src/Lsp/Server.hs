{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Lsp.Server (run) where

import Control.Concurrent.MVar qualified as MVar
import Control.Monad.IO.Class
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.State.Strict qualified as State
import Data.Text qualified as Text
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Lsp.Handlers
import Lsp.State

syncOptions :: LSP.TextDocumentSyncOptions
syncOptions =
  LSP.TextDocumentSyncOptions
    { _openClose = Just True,
      _change = Just LSP.TextDocumentSyncKind_Incremental,
      _willSave = Just False,
      _willSaveWaitUntil = Just False,
      _save = Just $ LSP.InR $ LSP.SaveOptions $ Just False
    }

lspOptions :: LSP.Options
lspOptions =
  LSP.defaultOptions
    { LSP.optTextDocumentSync = Just syncOptions,
      LSP.optExecuteCommandCommands = Just ["ants-ls"]
    }

run :: IO Int
run = do
  state <- MVar.newMVar initialState
  let interpreter :: LSP.LanguageContextEnv ServerConfig -> HandlerM LSP.<~> IO
      interpreter environment = LSP.Iso {..}
        where
          forward :: HandlerM a -> IO a
          forward handler =
            MVar.modifyMVar state \oldState -> do
              LSP.runLspT environment do
                (e, newState) <- State.runStateT (Except.runExceptT handler) oldState
                result <- case e of
                  Left (Log, _message) -> do
                    LSP.sendNotification LSP.SMethod_WindowLogMessage (LSP.LogMessageParams LSP.MessageType_Log _message)
                    liftIO (fail (Text.unpack _message))
                  Left (severity_, _message) -> do
                    let xtype = case severity_ of
                          Error -> LSP.MessageType_Error
                          Warning -> LSP.MessageType_Warning
                          Info -> LSP.MessageType_Info
                    LSP.sendNotification LSP.SMethod_WindowShowMessage (LSP.ShowMessageParams xtype _message)
                    liftIO (fail (Text.unpack _message))
                  Right a -> do
                    return a

                return (newState, result)
          backward = liftIO
  LSP.runServer $
    LSP.ServerDefinition
      { parseConfig = const $ const $ Right ServerConfig {},
        onConfigChange = const $ pure (),
        defaultConfig = ServerConfig {},
        configSection = "ants",
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = \_caps -> handlers,
        interpretHandler = interpreter,
        options = lspOptions
      }
