{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.DocFilterSpec (spec) where

import Data.Either
import qualified Data.Text as T
import Data.Time
import Model.DocFilter
import Model.Document
import Model.MarkdownAst
import qualified Model.Metadata as M
import Parser.Markdown
import Path
import Test.Hspec
import Text.RawString.QQ
import Prelude hiding (and, not, or)

testMarkdownDoc :: String
testMarkdownDoc =
  [r|
# Introduction

This is a Markdown document with a variety of elements for **testing purposes**.

## List

- Item 1
- Item 2
  - Subitem 2.1
  - Subitem 2.2

1. Ordered item 1
2. Ordered item 2


## Link

[Visit Haskell website](https://www.haskell.org)

## Image

![Haskell Logo](https://www.haskell.org/img/haskell-logo.svg)

> A blockquote with *emphasis*.

## Conclusion

Thank you for reading this document.

---
|]

keywordsDoc :: String
keywordsDoc =
  [r|# Sample Markdown Document

This document is for testing the keyword search functionality.

It contains some *keywords* that should be detected by the filter.

Here are some of the keywords we might want to find:
- Haskell
- Parsing
- Keyword
- Search
|]

strictKeywordsDoc :: String
strictKeywordsDoc =
  [r|
# Strict Keyword Search Document

This document is specifically for testing strict keyword search.

It contains exact phrases like "strict search" and "keyword detection".

Other phrases include "Haskell programming" and "text analysis".

|]

testDocWithLink :: String
testDocWithLink =
  [r|
# Document with Link
[somePlace](someLink/here/test.md)
|]

testDocWithoutLink :: String
testDocWithoutLink =
  [r|
# Document without Link
This document does not contain any links.
|]

testDocMultipleLinks :: String
testDocMultipleLinks =
  [r|
# Document with Multiple Links
[somePlace](someLink/here/test.md)
[anotherPlace](anotherLink/there/test.md)
|]

testMarkdownAst :: Maybe MarkdownAst
testMarkdownAst = fromRight Nothing $ markdownAst "test1" (T.pack testMarkdownDoc)

keywordsMarkdownAst :: Maybe MarkdownAst
keywordsMarkdownAst = fromRight Nothing $ markdownAst "test1" (T.pack keywordsDoc)

strictKeywordsMarkdownAst :: Maybe MarkdownAst
strictKeywordsMarkdownAst = fromRight Nothing $ markdownAst "test1" (T.pack strictKeywordsDoc)

testMarkdownAstWithLink :: Maybe MarkdownAst
testMarkdownAstWithLink = fromRight Nothing $ markdownAst "test1" (T.pack testDocWithLink)

testMarkdownAstWithoutLink :: Maybe MarkdownAst
testMarkdownAstWithoutLink = fromRight Nothing $ markdownAst "test1" (T.pack testDocWithoutLink)

testMarkdownAstMultipleLinks :: Maybe MarkdownAst
testMarkdownAstMultipleLinks = fromRight Nothing $ markdownAst "test1" (T.pack testDocMultipleLinks)

testMetadata :: M.Metadata
testMetadata =
  M.Metadata
    { M.title = Just "Test Title",
      M.author = Just "Test Author",
      M.dateTime = Just (parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-06-15T12:00:00Z"),
      M.tags = ["haskell", "parsing"],
      M.description = "This is a test document with some Haskell and parsing"
    }

testPath :: Path Rel File
testPath = $(mkRelFile "some/test/path/file.txt")

metadataSpec :: Spec
metadataSpec = describe "Metadata Filter" $ do
  let doc = Document testPath testMetadata testMarkdownAst (T.pack testMarkdownDoc)
  it "matches metadata with a specific date range" $ do
    let startDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-01-01T00:00:00Z"
    let endDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-12-31T23:59:59Z"
    let filter' = dateRange (Just startDate) (Just endDate)
    filt filter' doc `shouldBe` True

  it "does not match metadata outside of a specific date range" $ do
    let beforeDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2020-01-01T00:00:00Z"
    let afterDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-01-01T00:00:00Z"
    let filter' = dateRange (Just beforeDate) (Just afterDate)
    filt filter' doc `shouldBe` False

  it "matches metadata when only start date is given" $ do
    let startDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-01-01T00:00:00Z"
    let filter' = dateRange (Just startDate) Nothing
    filt filter' doc `shouldBe` True

  it "matches metadata when only end date is given" $ do
    let endDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-12-31T23:59:59Z"
    let filter' = dateRange Nothing (Just endDate)
    filt filter' doc `shouldBe` True

  it "matches metadata when no date range is given" $ do
    let filter' = dateRange Nothing Nothing
    filt filter' doc `shouldBe` True

  it "matches metadata with the specific author" $ do
    let filter' = author "Test Author"
    filt filter' doc `shouldBe` True

  it "does not match metadata with a different author" $ do
    let filter' = author "Nonexistent Author"
    filt filter' doc `shouldBe` False

  it "matches metadata with the specific title" $ do
    let filter' = title "Test Title"
    filt filter' doc `shouldBe` True

  it "does not match metadata with a different title" $ do
    let filter' = title "Nonexistent Title"
    filt filter' doc `shouldBe` False

  it "matches metadata with a specific tag" $ do
    let filter' = hasTag "haskell"
    filt filter' doc `shouldBe` True

  it "does not match metadata without a specific tag" $ do
    let filter' = hasTag "nonexistent"
    filt filter' doc `shouldBe` False

  it "matches metadata with fuzzy description" $ do
    let filter' = fuzzyDescription "Haskell parsing"
    filt filter' doc `shouldBe` True

  it "matches metadata that does not contain the fuzzy description" $ do
    let filter' = fuzzyDescription "unrelated topic"
    filt filter' doc `shouldBe` False

  it "matches metadata when fuzzy description is partially matched" $ do
    let filter' = fuzzyDescription "test document"
    filt filter' doc `shouldBe` True

  it "does not match metadata when fuzzy description is not matched at all" $ do
    let filter' = fuzzyDescription "completely unrelated"
    filt filter' doc `shouldBe` False

  it "matches metadata with case insensitive fuzzy description" $ do
    let filter' = fuzzyDescription "HASKELL PARSING"
    filt filter' doc `shouldBe` True

  it "matches metadata when keyword is found in MarkdownAst" $ do
    let filter' = keyword "world"
    filt filter' doc `shouldBe` True

  it "does not match metadata when keyword is not found in MarkdownAst" $ do
    let filter' = keyword "nonexistent"
    filt filter' doc `shouldBe` False

keywordsFilterSpec :: Spec
keywordsFilterSpec = describe "Keywords Filter" $ do
  let doc = Document testPath testMetadata keywordsMarkdownAst (T.pack keywordsDoc)
  it "matches a single keyword present in the document" $ do
    let filter' = keywords ["Haskell"]
    filt filter' doc `shouldBe` True

  it "matches any of multiple keywords present in the document" $ do
    let filter' = keywords ["Haskell", "JavaScript", "Python"]
    filt filter' doc `shouldBe` True

  it "does not match a keyword that is not present in the document" $ do
    let filter' = keywords ["Nonexistent"]
    filt filter' doc `shouldBe` False

  it "matches keywords with mixed case in the document" $ do
    let filter' = keywords ["haskell", "parsing"]
    filt filter' doc `shouldBe` True

  it "does not match when none of the multiple keywords are present" $ do
    let filter' = keywords ["Java", "C++"]
    filt filter' doc `shouldBe` False

  it "matches partial keywords if they are part of a word in the document" $ do
    let filter' = keywords ["key", "pars"]
    filt filter' doc `shouldBe` True

  it "matches keywords in a list within the document" $ do
    let filter' = keywords ["Parsing", "Search"]
    filt filter' doc `shouldBe` True

strictKeywordsFilterSpec :: Spec
strictKeywordsFilterSpec = describe "Strict Keywords Filter" $ do
  let doc = Document testPath testMetadata strictKeywordsMarkdownAst (T.pack strictKeywordsDoc)
  it "matches an exact keyword in the document" $ do
    let filter' = strictKeyword "strict search"
    filt filter' doc `shouldBe` True

  it "does not match a keyword if not an exact match" $ do
    let filter' = strictKeyword "nonexist"
    filt filter' doc `shouldBe` False

  it "matches one of the keywords exactly in the document" $ do
    let filter' = strictKeywords ["nonexistent", "Haskell programming"]
    filt filter' doc `shouldBe` True

  it "does not match when none of the exact keywords are present" $ do
    let filter' = strictKeywords ["Ruby", "Java", "C++"]
    filt filter' doc `shouldBe` False

  it "matches all exact keywords present in the document" $ do
    let filter' = strictKeywords ["strict search", "keyword detection"]
    filt filter' doc `shouldBe` True

  it "does not match partial keywords" $ do
    let filter' = strictKeywords ["stricted", "searching"]
    filt filter' doc `shouldBe` False

  it "matches exact keywords regardless of their location in the document" $ do
    let filter' = strictKeywords ["text analysis"]
    filt filter' doc `shouldBe` True

  it "matches exact phrases including special characters" $ do
    let filter' = strictKeyword "\"strict search\""
    filt filter' doc `shouldBe` True

  it "does not match keywords if not exactly present (case-sensitive)" $ do
    let filter' = strictKeywords ["Strict Search", "haskell Programming"]
    filt filter' doc `shouldBe` False

pathFilterSpec :: Spec
pathFilterSpec = do
  describe "Path Matching Filter" $ do
    let doc = Document testPath testMetadata testMarkdownAst (T.pack testMarkdownDoc)

    it "matches document with specific relative path" $ do
      let filter' = matchRelPath $(mkRelFile "some/test/path/file.txt")
      filt filter' doc `shouldBe` True

    it "does not match document with different relative path" $ do
      let filter' = matchRelPath $(mkRelFile "other/test/path/file.txt")
      filt filter' doc `shouldBe` False

  describe "Match Relative Paths Filter" $ do
    let doc1Path = $(mkRelFile "docs/complexTest.md")
    let doc2Path = $(mkRelFile "docs/anotherTest.md")
    let doc3Path = $(mkRelFile "docs/yetAnotherTest.md")
    let doc1 = Document doc1Path testMetadata testMarkdownAst (T.pack testMarkdownDoc)
    let doc2 = Document doc2Path testMetadata testMarkdownAst (T.pack testMarkdownDoc)
    let doc3 = Document doc3Path testMetadata testMarkdownAst (T.pack testMarkdownDoc)

    it "matches a document with one of the specified paths" $ do
      let paths = [doc1Path, doc2Path]
      let filter' = matchRelPaths paths
      filt filter' doc1 `shouldBe` True
      filt filter' doc2 `shouldBe` True

    it "does not match a document if its path is not in the list" $ do
      let paths = [doc1Path, doc2Path]
      let filter' = matchRelPaths paths
      filt filter' doc3 `shouldBe` False

    it "matches documents with any of the multiple specified paths" $ do
      let paths = [doc1Path, doc2Path, doc3Path]
      let filter' = matchRelPaths paths
      filt filter' doc1 `shouldBe` True
      filt filter' doc2 `shouldBe` True
      filt filter' doc3 `shouldBe` True

    it "matches no documents if the list of paths is empty" $ do
      let paths = []
      let filter' = matchRelPaths paths
      filt filter' doc1 `shouldBe` False
      filt filter' doc2 `shouldBe` False
      filt filter' doc3 `shouldBe` False

linkFilterSpec :: Spec
linkFilterSpec = do
  describe "Has Link Filter" $ do
    let docWithLink = Document testPath testMetadata testMarkdownAstWithLink (T.pack testDocWithLink)
    let docWithoutLink = Document testPath testMetadata testMarkdownAstWithoutLink (T.pack testDocWithoutLink)

    let linkInDoc = $(mkRelFile "someLink/here/test.md")
    let nonExistentLink = $(mkRelFile "nonexistent/link.md")

    it "matches document containing a specific link" $ do
      let filter' = hasLink linkInDoc
      filt filter' docWithLink `shouldBe` True

    it "does not match document without the specific link" $ do
      let filter' = hasLink linkInDoc
      filt filter' docWithoutLink `shouldBe` False

    it "does not match document with a non-existent link" $ do
      let filter' = hasLink nonExistentLink
      filt filter' docWithLink `shouldBe` False

    it "matches document with multiple links if one of them matches" $ do
      let multipleLinksDoc = Document testPath testMetadata testMarkdownAstMultipleLinks (T.pack testDocMultipleLinks)
      let filter' = hasLink linkInDoc
      filt filter' multipleLinksDoc `shouldBe` True

spec :: Spec
spec = do
  metadataSpec
  keywordsFilterSpec
  strictKeywordsFilterSpec
  pathFilterSpec
  linkFilterSpec
