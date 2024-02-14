{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Path
import Path.IO
import System.Process
import Test.Hspec
import Text.RawString.QQ

sampleMarkdownDoc :: String
sampleMarkdownDoc =
  [r|# Introduction
  This is a sample document.
  |]

hoverSpec :: Spec
hoverSpec = describe "Lsp Hover" $ do
  it "works" $ do
    withSystemTempDir "docLoader" $ \tmp -> do
      writeFile (toFilePath (tmp </> $(mkRelFile "test.md"))) sampleMarkdownDoc
      runSession "ants-ls" fullCaps (toFilePath tmp) $
        do
          docId <- openDoc "test.md" "markdown"
          hover <- getHover docId (Position 0 5)
          liftIO $ do
            hover `shouldSatisfy` isJust
            pure ()

spec :: Spec
spec = sequential $ do
  hoverSpec
