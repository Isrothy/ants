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

import Cli.InitNoteBook (initNotebook)
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
import Parser.Opts
import Path
import Path.IO
import Project.ProjectRoot
import System.Process
import Test.Hspec
import Text.RawString.QQ

sampleMarkdownDoc :: String
sampleMarkdownDoc =
  [r|# Introduction

This is a sample document.

[this is a link](target.md#tag)

[[target.md#tag]]
  |]

configWithoutWikilink :: String
configWithoutWikilink =
  [r|
{
  "template":{
    "name": "John",
    "email": "john@example.com",
    "dateFormat": "%Y-%m-%d",
    "timeFormat": "%H:%M:%S",
    "dateTimeFormat": "%Y-%m-%dT%H:%M:%S",
    "variables": {
      "game": "League of Legends"
    }
  },
  "extensions": [
    "gfm", "attributes", "auto_identifiers"
  ]
}
|]

configWithWikilink :: String
configWithWikilink =
  [r|
{
  "template":{
    "name": "John",
    "email": "john@example.com",
    "dateFormat": "%Y-%m-%d",
    "timeFormat": "%H:%M:%S",
    "dateTimeFormat": "%Y-%m-%dT%H:%M:%S",
    "variables": {
      "game": "League of Legends"
    }
  },
  "extensions": [
    "gfm", "attributes", "auto_identifiers", "wikilinks_title_after_pipe"
  ]
}
|]

hoverSpec :: Spec
hoverSpec = describe "Lsp Hover" $ sequential $ do
  it "do not hover on none link" $ do
    withSystemTempDir "LspHover" $ \tmp -> do
      cur <- getCurrentDir
      let setup = do
            setCurrentDir tmp
            initNotebook InitOptions
            writeFile (toFilePath (tmp </> $(mkRelFile "test.md"))) sampleMarkdownDoc
          cleanup = do
            setCurrentDir cur
      bracket_ setup cleanup $ do
        runSession "ants-ls" fullCaps (toFilePath tmp) $
          do
            docId <- openDoc "test.md" "markdown"
            hover <- getHover docId (Position 0 5)
            liftIO $ do
              hover `shouldSatisfy` isNothing
              pure ()

  it "hover on link" $ do
    withSystemTempDir "LspHover" $ \tmp -> do
      cur <- getCurrentDir
      let setup = do
            setCurrentDir tmp
            initNotebook InitOptions
            writeFile (toFilePath (tmp </> $(mkRelFile "test.md"))) sampleMarkdownDoc
          cleanup = do
            setCurrentDir cur
      bracket_ setup cleanup $ do
        runSession "ants-ls" fullCaps (toFilePath tmp) $
          do
            docId <- openDoc "test.md" "markdown"
            hover <- getHover docId (Position 4 5)
            liftIO $ do
              hover `shouldSatisfy` isJust
              pure ()

  it "respects config" $ do
    withSystemTempDir "LspHover" $ \tmp -> do
      cur <- getCurrentDir
      let setup = do
            setCurrentDir tmp
            initNotebook InitOptions
            writeFile (toFilePath $ tmp </> configDir </> configFileName) configWithoutWikilink
            writeFile (toFilePath (tmp </> $(mkRelFile "test.md"))) sampleMarkdownDoc
          cleanup = do
            setCurrentDir cur
      bracket_ setup cleanup $ do
        runSession "ants-ls" fullCaps (toFilePath tmp) $
          do
            docId <- openDoc "test.md" "markdown"
            hover <- getHover docId (Position 6 5)
            liftIO $ do
              hover `shouldSatisfy` isNothing
              pure ()

  it "hover on wiki-link" $ do
    withSystemTempDir "LspHover" $ \tmp -> do
      cur <- getCurrentDir
      let setup = do
            setCurrentDir tmp
            initNotebook InitOptions
            writeFile (toFilePath $ tmp </> configDir </> configFileName) configWithWikilink
            writeFile (toFilePath (tmp </> $(mkRelFile "test.md"))) sampleMarkdownDoc
          cleanup = do
            setCurrentDir cur
      bracket_ setup cleanup $ do
        runSession "ants-ls" fullCaps (toFilePath tmp) $
          do
            docId <- openDoc "test.md" "markdown"
            hover <- getHover docId (Position 6 5)
            liftIO $ do
              hover `shouldSatisfy` isJust
              pure ()

spec :: Spec
spec = sequential $ do
  hoverSpec
