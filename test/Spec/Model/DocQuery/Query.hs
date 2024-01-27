{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Model.DocQuery.Query (spec) where

import Data.Either
import qualified Data.Text as T
import Data.Time
import Model.DocQuery
import Model.Document
import Model.MarkdownAst
import qualified Model.Metadata as M
import Parser.Markdown
import Path
import Test.Hspec
import Text.RawString.QQ
import Prelude hiding (and, any, not, or)

sampleMarkdownDoc :: String
sampleMarkdownDoc =
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

sampleMarkdownAst :: Maybe MarkdownAst
sampleMarkdownAst = fromRight Nothing $ markdownAst "test1" (T.pack sampleMarkdownDoc)

sampleMetadata :: M.Metadata
sampleMetadata =
  M.Metadata
    { M.title = Just "Test Title",
      M.author = Just "Test Author",
      M.dateTime = Just (parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-06-15T12:00:00Z"),
      M.tags = ["haskell", "parsing"],
      M.description = "This is a test document with some Haskell and parsing"
    }

samplePath :: Path Rel File
samplePath = $(mkRelFile "some/test/path/file.txt")

metadataQuerySpec :: Spec
metadataQuerySpec = describe "Metadata Filter" $ do
  let doc = Document samplePath sampleMetadata sampleMarkdownAst (T.pack sampleMarkdownDoc)
  it "matches metadata with a specific date range" $ do
    let startDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-01-01T00:00:00Z"
    let endDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-12-31T23:59:59Z"
    let filter' = DateRange (Just startDate) (Just endDate)
    query filter' doc `shouldBe` True

  it "does not match metadata outside of a specific date range" $ do
    let beforeDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2020-01-01T00:00:00Z"
    let afterDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-01-01T00:00:00Z"
    let filter' = DateRange (Just beforeDate) (Just afterDate)
    query filter' doc `shouldBe` False

  it "matches metadata when only start date is given" $ do
    let startDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-01-01T00:00:00Z"
    let filter' = DateRange (Just startDate) Nothing
    query filter' doc `shouldBe` True

  it "matches metadata when only end date is given" $ do
    let endDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-12-31T23:59:59Z"
    let filter' = DateRange Nothing (Just endDate)
    query filter' doc `shouldBe` True

  it "matches metadata when no date range is given" $ do
    let filter' = DateRange Nothing Nothing
    query filter' doc `shouldBe` True

  it "matches metadata with the specific author" $ do
    let filter' = Author $ Val $ StrictTerm "Test Author"
    query filter' doc `shouldBe` True

  it "does not match metadata with a different author" $ do
    let filter' = Author $ Val $ StrictTerm "Nonexistent Author"
    query filter' doc `shouldBe` False

  it "matches metadata with the specific title" $ do
    let filter' = Title $ Val $ StrictTerm "Test Title"
    query filter' doc `shouldBe` True

  it "does not match metadata with a different title" $ do
    let filter' = Title $ Val $ StrictTerm "Nonexistent Title"
    query filter' doc `shouldBe` False

  it "matches metadata with a specific tag" $ do
    let filter' = Tag $ Val $ StrictTerm "haskell"
    query filter' doc `shouldBe` True

  it "does not match metadata without a specific tag" $ do
    let filter' = Tag $ Val $ StrictTerm "nonexistent"
    query filter' doc `shouldBe` False

  it "matches metadata with fuzzy description" $ do
    let filter' = Description $ Val $ FuzzyTerm "Haskell and * parsing"
    query filter' doc `shouldBe` True

  it "matches metadata that does not contain the fuzzy description" $ do
    let filter' = Description $ Val $ FuzzyTerm "unrelated topic"
    query filter' doc `shouldBe` False

  it "matches metadata when fuzzy description is partially matched" $ do
    let filter' = Description $ Val $ FuzzyTerm "test document"
    query filter' doc `shouldBe` True

  it "does not match metadata when fuzzy description is not matched at all" $ do
    let filter' = Description $ Val $ FuzzyTerm "completely unrelated"
    query filter' doc `shouldBe` False

  it "matches metadata with case insensitive fuzzy description" $ do
    let filter' = Description $ Val $ FuzzyTerm "HASKELL AND PARSING"
    query filter' doc `shouldBe` True

pathQuerySpec :: Spec
pathQuerySpec = do
  describe "Path Matching Filter" $ do
    let doc = Document samplePath sampleMetadata sampleMarkdownAst (T.pack sampleMarkdownDoc)

    it "matches document with specific relative path" $ do
      query (InDirectory $(mkRelDir "some/test/path")) doc `shouldBe` True
      query (InDirectory $(mkRelDir "some/test")) doc `shouldBe` True

    it "does not match document with different relative path" $ do
      query (InDirectory $(mkRelDir "other/test/path")) doc `shouldBe` False

linkQuerySpec :: Spec
linkQuerySpec = do
  let testDocWithoutLink =
        [r|
# Document without Link
This document does not contain any links.
|]
  let testDocWithLink =
        [r|
# Document with Link
[somePlace](someLink/here/test.md)
|]
  let testDocMultipleLinks =
        [r|
# Document with Multiple Links
[somePlace](someLink/here/test.md)
[anotherPlace](anotherLink/there/test.md)
|]

  let testMarkdownAstWithLink = fromRight Nothing $ markdownAst "test1" (T.pack testDocWithLink)
  let testMarkdownAstWithoutLink = fromRight Nothing $ markdownAst "test1" (T.pack testDocWithoutLink)
  let testMarkdownAstMultipleLinks = fromRight Nothing $ markdownAst "test1" (T.pack testDocMultipleLinks)
  let docWithLink = Document samplePath sampleMetadata testMarkdownAstWithLink (T.pack testDocWithLink)
  let docWithoutLink = Document samplePath sampleMetadata testMarkdownAstWithoutLink (T.pack testDocWithoutLink)
  let multipleLinksDoc = Document samplePath sampleMetadata testMarkdownAstMultipleLinks (T.pack testDocMultipleLinks)
  let linkInDoc = $(mkRelFile "someLink/here/test.md")
  let nonExistentLink = $(mkRelFile "nonexistent/link.md")

  describe "Has Link Filter" $ do
    it "matches document containing a specific link" $ do
      let filter' = HasLink linkInDoc
      query filter' docWithLink `shouldBe` True

    it "does not match document without the specific link" $ do
      let filter' = HasLink linkInDoc
      query filter' docWithoutLink `shouldBe` False

    it "does not match document with a non-existent link" $ do
      let filter' = HasLink nonExistentLink
      query filter' docWithLink `shouldBe` False

    it "matches document with multiple links if one of them matches" $ do
      let filter' = HasLink linkInDoc
      query filter' multipleLinksDoc `shouldBe` True

contentQuerySpec :: Spec
contentQuerySpec = parallel $ do
  describe "Complex Content Query Functionality" $ parallel $ do
    it "matches a document with content that satisfies both terms in an AND query" $ do
      let doc = Document samplePath sampleMetadata sampleMarkdownAst $ T.pack sampleMarkdownDoc
      let query' = Content (And (Val (StrictTerm "Markdown")) (Val (FuzzyTerm "iteem")))
      query query' doc `shouldBe` True

    it "does not match a document when one term in an AND query is not satisfied" $ do
      let doc = Document samplePath sampleMetadata sampleMarkdownAst $ T.pack sampleMarkdownDoc
      let query' = Content (And (Val (StrictTerm "markdown")) (Val (FuzzyTerm "test")))
      query query' doc `shouldBe` False

    it "matches a document with content that satisfies at least one term in an OR query" $ do
      let doc = Document samplePath sampleMetadata sampleMarkdownAst $ T.pack sampleMarkdownDoc
      let query' = Content (Or (Val (StrictTerm "markdown")) (Val (StrictTerm "Ordered")))
      query query' doc `shouldBe` True

    it "does not match a document when none of the terms in an OR query are satisfied" $ do
      let doc = Document samplePath sampleMetadata sampleMarkdownAst $ T.pack sampleMarkdownDoc
      let query' = Content (Or (Val (StrictTerm "markup")) (Val (StrictTerm "markdown")))
      query query' doc `shouldBe` False

spec :: Spec
spec = parallel $ do
  metadataQuerySpec
  pathQuerySpec
  linkQuerySpec
  contentQuerySpec
