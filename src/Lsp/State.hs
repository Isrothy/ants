{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lsp.State where

import Commonmark (SyntaxSpec)
import Commonmark.Syntax (defaultSyntaxSpec)
import Control.Lens.TH (makeLenses)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Aeson
  ( FromJSON (..),
    withObject,
  )
import Data.Default (Default (def))
import Data.Functor.Identity (Identity)
import qualified Data.Text as T
import qualified Language.LSP.Server as LSP
import Model.MarkdownAst
import Parser.Markdown

type HandlerM =
  ExceptT (Severity, T.Text) (StateT ServerState (LSP.LspT ServerConfig IO))

data Severity
  = Error
  | Warning
  | Info
  | Log

data ServerConfig where
  ServerConfig :: {} -> ServerConfig

instance Default ServerConfig where
  def = ServerConfig

instance FromJSON ServerConfig where
  parseJSON = withObject "settings" $ \o -> do
    return def

data ServerState where
  ServerState ::
    { _markdownSyntaxSpec :: MarkdownSyntax
    } ->
    ServerState

makeLenses ''ServerState

initialState :: ServerState
initialState =
  ServerState
    { _markdownSyntaxSpec = defaultSyntaxSpec
    }
