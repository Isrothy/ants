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
import Language.LSP.Server
import Language.LSP.Server qualified as LSP
import Lsp.Handlers
import Lsp.State

run :: IO Int
run = do
  state <- MVar.newMVar initialState
  let interpreter :: LanguageContextEnv ServerConfig -> HandlerM <~> IO
      interpreter environment = Iso {..}
        where
          forward :: HandlerM a -> IO a
          forward handler =
            MVar.modifyMVar state \oldState -> do
              LSP.runLspT environment do
                (e, newState) <- State.runStateT (Except.runExceptT handler) oldState
                result <- case e of
                  Left (Log, _message) -> do
                    -- let _xtype = MtLog

                    -- LSP.sendNotification SWindowLogMessage LogMessageParams {..}

                    liftIO (fail (Text.unpack _message))
                  Left (severity_, _message) -> do
                    -- let _xtype = case severity_ of
                    --       Error -> MtError
                    --       Warning -> MtWarning
                    --       Info -> MtInfo
                    --       Log -> MtLog

                    -- LSP.sendNotification SWindowShowMessage ShowMessageParams {..}
                    liftIO (fail (Text.unpack _message))
                  Right a -> do
                    return a

                return (newState, result)
          backward = liftIO
  runServer $
    ServerDefinition
      { parseConfig = const $ const $ Right ServerConfig,
        onConfigChange = const $ pure (),
        defaultConfig = ServerConfig,
        configSection = "demo",
        doInitialize = \env _req -> pure $ Right env,
        staticHandlers = \_caps -> handlers,
        interpretHandler = interpreter,
        options = defaultOptions
      }
