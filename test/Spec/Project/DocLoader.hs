{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Project.DocLoader (spec) where

import Commonmark
import Control.Exception
import qualified Data.Text as T
import qualified Model.Document as D
import Path
import Path.IO
import Project.DocLoader
import Test.Hspec
import Text.RawString.QQ

sampleMarkdownDoc :: String
sampleMarkdownDoc =
  [r|
# Introduction
  This is a sample document.
  |]

sampleMarkdownDoc2 :: String
sampleMarkdownDoc2 =
  [r|
# Introduction
  This is another sample document.
  |]

spec :: Spec
spec = describe "document loading" $ sequential $ do
  it "loads a single document correctly" $ do
    withSystemTempDir "docLoader" $ \tmp -> do
      cur <- getCurrentDir
      let setup = do
            writeFile (toFilePath (tmp </> $(mkRelFile "test.md"))) sampleMarkdownDoc
            setCurrentDir tmp
          cleanup = do
            setCurrentDir cur
      bracket_ setup cleanup $ do
        doc <- loadDocument defaultSyntaxSpec $(mkRelDir ".") $(mkRelFile "test.md")
        T.unpack (D.text doc) `shouldBe` sampleMarkdownDoc
        D.filename doc `shouldBe` "test.md"

  it "loads a single document in some directory correctly" $ do
    withSystemTempDir "docLoader" $ \tmp -> do
      cur <- getCurrentDir
      let setup = do
            ensureDir (tmp </> $(mkRelDir "some/sub/dir"))
            writeFile (toFilePath (tmp </> $(mkRelFile "some/sub/dir/test.md"))) sampleMarkdownDoc
            setCurrentDir tmp
          cleanup = do
            setCurrentDir cur
      bracket_ setup cleanup $ do
        doc <- loadDocument defaultSyntaxSpec $(mkRelDir "some/sub") $(mkRelFile "dir/test.md")
        T.unpack (D.text doc) `shouldBe` sampleMarkdownDoc

  it "loads all documents in some directory correctly" $ do
    withSystemTempDir "docLoader" $ \tmp -> do
      cur <- getCurrentDir
      let setup = do
            ensureDir (tmp </> $(mkRelDir "some/sub/dir"))
            writeFile (toFilePath (tmp </> $(mkRelFile "test1.md"))) sampleMarkdownDoc
            writeFile (toFilePath (tmp </> $(mkRelFile "some/sub/dir/test2.md"))) sampleMarkdownDoc2
            writeFile (toFilePath (tmp </> $(mkRelFile "test3"))) sampleMarkdownDoc
            writeFile (toFilePath (tmp </> $(mkRelFile "some/sub/test4"))) sampleMarkdownDoc2
            setCurrentDir tmp
          cleanup = do
            setCurrentDir cur
      bracket_ setup cleanup $ do
        docs <- loadAllFromDirectory defaultSyntaxSpec $(mkRelDir ".")
        length docs `shouldBe` 2
