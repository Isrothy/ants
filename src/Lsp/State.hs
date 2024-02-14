{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lsp.State where

import Control.Lens.TH (makeLenses)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Aeson
  ( FromJSON (..),
    withObject,
  )
import Data.Default (Default (def))
import qualified Data.Text as T
import Language.LSP.Server

type HandlerM =
  ExceptT (Severity, T.Text) (StateT ServerState (LspT ServerConfig IO))

data Severity
  = Error
  | Warning
  | Info
  | Log

data ServerConfig = ServerConfig
  {
  }

instance Default ServerConfig where
  def = ServerConfig

instance FromJSON ServerConfig where
  parseJSON = withObject "server-settings" $ \o -> do
    return def

data ServerState = ServerState {}

makeLenses ''ServerState

initialState :: ServerState
initialState = ServerState {}
