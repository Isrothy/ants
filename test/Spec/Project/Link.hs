{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Project.Link (spec) where

import Data.Maybe
import qualified Data.Text as T
import Parser.Markdown
import Path
import Path.IO
import Project.DocLoader
import Project.Link
import Test.Hspec
import Text.RawString.QQ

sampleMarkdownDoc :: String
sampleMarkdownDoc =
  [r|
# Introduction
  This is a sample document.
  |]

sampleMarkdownDocWithBookmark :: String
sampleMarkdownDocWithBookmark =
  [r|
# Introduction {#tag}
  This is a sample document.
  |]

resolveLinkInFileSpec :: Spec
resolveLinkInFileSpec = describe "resolveLinkInFile" $ sequential $ do
  it "follows an absolute file path correctly" $ do
    withSystemTempDir "test" $ \dir -> do
      let orig = dir </> $(mkRelFile "orig.md")
      let absPath = dir </> $(mkRelFile "test.md")
      resolveLinkInFile orig (toFilePath absPath) `shouldReturn` Just absPath

  it "follows a relative file path correctly" $ do
    withSystemTempDir "test" $ \dir -> do
      ensureDir (dir </> $(mkRelDir "some/sub/dir"))
      let orig = dir </> $(mkRelFile "some/sub/dir/orig.md")
      resolveLinkInFile orig "../test.md" `shouldReturn` Just (dir </> $(mkRelFile "some/sub/test.md"))

  it "is unchanged if link is empty" $ do
    withSystemTempDir "test" $ \dir -> do
      ensureDir (dir </> $(mkRelDir "some/sub/dir"))
      let orig = dir </> $(mkRelFile "some/sub/dir/orig.md")
      resolveLinkInFile orig "" `shouldReturn` Just orig

gotoLinkedElementSpec :: Spec
gotoLinkedElementSpec = describe "gotoLinkedElement" $ sequential $ do
  it "gives nothing when path not exists" $ do
    withSystemTempDir "test" $ \dir -> do
      let orig = dir </> $(mkRelFile "orig.md")
      let linked = dir </> $(mkRelFile "linked.md")
      gotoLinkedElement allSpecExtensions dir orig (T.pack (toFilePath linked))
        `shouldReturn` Nothing

  it "resolves a absolute link to an existing file without a bookmark" $ do
    withSystemTempDir "test" $ \dir -> do
      let orig = dir </> $(mkRelFile "orig.md")
      let linked = dir </> $(mkRelFile "linked.md")
      writeFile (toFilePath linked) sampleMarkdownDoc
      doc <- loadDocument allSpecExtensions dir $(mkRelFile "linked.md")
      gotoLinkedElement allSpecExtensions dir orig (T.pack (toFilePath linked))
        `shouldReturn` Just (doc, Nothing)

  it "resolves a relative link to an existing file without a bookmark" $ do
    withSystemTempDir "test" $ \dir -> do
      ensureDir (dir </> $(mkRelDir "some/sub/dir"))
      let orig = dir </> $(mkRelFile "some/sub/orig.md")
      let linked = dir </> $(mkRelFile "some/sub/dir/linked.md")
      writeFile (toFilePath linked) sampleMarkdownDoc
      doc <- loadDocument allSpecExtensions dir $(mkRelFile "some/sub/dir/linked.md")
      gotoLinkedElement allSpecExtensions dir orig "../sub/dir/linked.md"
        `shouldReturn` Just (doc, Nothing)

  it "resolves a absolute link to an existing file with a bookmark" $ do
    withSystemTempDir "test" $ \dir -> do
      let orig = dir </> $(mkRelFile "orig.md")
      let linked = dir </> $(mkRelFile "linked.md")
      writeFile (toFilePath linked) sampleMarkdownDocWithBookmark
      ret <- gotoLinkedElement allSpecExtensions dir orig (T.pack (toFilePath linked ++ "#tag"))
      case ret of
        Nothing -> expectationFailure "should return something"
        Just (_, ast) -> ast `shouldSatisfy` isJust

  it "goto the bookmark of current file" $ do
    withSystemTempDir "test" $ \dir -> do
      let orig = dir </> $(mkRelFile "orig.md")
      writeFile (toFilePath orig) sampleMarkdownDocWithBookmark
      ret <- gotoLinkedElement allSpecExtensions dir orig (T.pack "#tag")
      case ret of
        Nothing -> expectationFailure "should return something"
        Just (_, ast) -> ast `shouldSatisfy` isJust

  it "resolves a absolute link to an existing file with a wrong bookmark" $ do
    withSystemTempDir "test" $ \dir -> do
      let orig = dir </> $(mkRelFile "orig.md")
      let linked = dir </> $(mkRelFile "linked.md")
      writeFile (toFilePath linked) sampleMarkdownDocWithBookmark
      ret <- gotoLinkedElement allSpecExtensions dir orig (T.pack (toFilePath linked ++ "#no-tag"))
      case ret of
        Nothing -> expectationFailure "should return something"
        Just (_, ast) -> ast `shouldSatisfy` isNothing

spec :: Spec
spec = sequential $ do
  resolveLinkInFileSpec
  gotoLinkedElementSpec
