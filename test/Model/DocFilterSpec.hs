{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Model.DocFilterSpec (spec) where

import Data.Either
import qualified Data.Text as T
import Data.Time
import Model.DocFilter
import Model.MarkdownAst
import qualified Model.Metadata as M
import Parser.Markdown
import Test.Hspec
import Text.RawString.QQ
import Util.Filter
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

testMetadata :: M.Metadata
testMetadata =
  M.Metadata
    { M.title = Just "Test Title",
      M.author = Just "Test Author",
      M.dateTime = Just (parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-06-15T12:00:00Z"),
      M.tags = ["haskell", "parsing"],
      M.description = "This is a test document with some Haskell and parsing"
    }

testMarkdownAst :: Maybe MarkdownAst
testMarkdownAst = fromRight Nothing $ markdownAst "test1" (T.pack testMarkdownDoc)

keywordsMarkdownAst :: Maybe MarkdownAst
keywordsMarkdownAst = fromRight Nothing $ markdownAst "test1" (T.pack keywordsDoc)

strictKeywordsMarkdownAst :: Maybe MarkdownAst
strictKeywordsMarkdownAst = fromRight Nothing $ markdownAst "test1" (T.pack strictKeywordsDoc)

metadataSpec :: Spec
metadataSpec = describe "Metadata Filter" $ do
  it "matches metadata with a specific date range" $ do
    let startDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-01-01T00:00:00Z"
    let endDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-12-31T23:59:59Z"
    let filter' = dateRange (Just startDate) (Just endDate)
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` True

  it "does not match metadata outside of a specific date range" $ do
    let beforeDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2020-01-01T00:00:00Z"
    let afterDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-01-01T00:00:00Z"
    let filter' = dateRange (Just beforeDate) (Just afterDate)
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` False

  it "matches metadata when only start date is given" $ do
    let startDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-01-01T00:00:00Z"
    let filter' = dateRange (Just startDate) Nothing
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` True

  it "matches metadata when only end date is given" $ do
    let endDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-12-31T23:59:59Z"
    let filter' = dateRange Nothing (Just endDate)
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` True

  it "matches metadata when no date range is given" $ do
    let filter' = dateRange Nothing Nothing
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` True

  it "matches metadata with the specific author" $ do
    let filter' = author "Test Author"
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` True

  it "does not match metadata with a different author" $ do
    let filter' = author "Nonexistent Author"
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` False

  it "matches metadata with the specific title" $ do
    let filter' = title "Test Title"
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` True

  it "does not match metadata with a different title" $ do
    let filter' = title "Nonexistent Title"
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` False

  it "matches metadata with a specific tag" $ do
    let filter' = hasTag "haskell"
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` True

  it "does not match metadata without a specific tag" $ do
    let filter' = hasTag "nonexistent"
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` False

  it "matches metadata with fuzzy description" $ do
    let filter' = fuzzyDescription "Haskell parsing"
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` True

  it "matches metadata that does not contain the fuzzy description" $ do
    let filter' = fuzzyDescription "unrelated topic"
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` False

  it "matches metadata when fuzzy description is partially matched" $ do
    let filter' = fuzzyDescription "test document"
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` True

  it "does not match metadata when fuzzy description is not matched at all" $ do
    let filter' = fuzzyDescription "completely unrelated"
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` False

  it "matches metadata with case insensitive fuzzy description" $ do
    let filter' = fuzzyDescription "HASKELL PARSING"
    filt filter' (testMetadata, testMarkdownAst) `shouldBe` True

  it "matches metadata when keyword is found in MarkdownAst" $ do
    let filter' = keyword "world"
    filt filter' (testMetadata, keywordsMarkdownAst) `shouldBe` True

  it "does not match metadata when keyword is not found in MarkdownAst" $ do
    let filter' = keyword "nonexistent"
    filt filter' (testMetadata, keywordsMarkdownAst) `shouldBe` False

keywordsFilterSpec :: Spec
keywordsFilterSpec = do
  describe "Keywords Filter" $ do
    it "matches a single keyword present in the document" $ do
      let filter' = keywords ["Haskell"]
      filt filter' (testMetadata, keywordsMarkdownAst) `shouldBe` True

    it "matches any of multiple keywords present in the document" $ do
      let filter' = keywords ["Haskell", "JavaScript", "Python"]
      filt filter' (testMetadata, keywordsMarkdownAst) `shouldBe` True

    it "does not match a keyword that is not present in the document" $ do
      let filter' = keywords ["Nonexistent"]
      filt filter' (testMetadata, keywordsMarkdownAst) `shouldBe` False

    it "matches keywords with mixed case in the document" $ do
      let filter' = keywords ["haskell", "parsing"]
      filt filter' (testMetadata, keywordsMarkdownAst) `shouldBe` True

    it "does not match when none of the multiple keywords are present" $ do
      let filter' = keywords ["Java", "C++"]
      filt filter' (testMetadata, keywordsMarkdownAst) `shouldBe` False

    it "matches partial keywords if they are part of a word in the document" $ do
      let filter' = keywords ["key", "pars"]
      filt filter' (testMetadata, keywordsMarkdownAst) `shouldBe` True

    it "matches keywords in a list within the document" $ do
      let filter' = keywords ["Parsing", "Search"]
      filt filter' (testMetadata, keywordsMarkdownAst) `shouldBe` True

strictKeywordsFilterSpec :: Spec
strictKeywordsFilterSpec = do
  describe "Strict Keywords Filter" $ do
    it "matches an exact keyword in the document" $ do
      let filter' = strictKeyword "strict search"
      filt filter' (testMetadata, strictKeywordsMarkdownAst) `shouldBe` True

    it "does not match a keyword if not an exact match" $ do
      let filter' = strictKeyword "nonexist"
      filt filter' (testMetadata, strictKeywordsMarkdownAst) `shouldBe` False

    it "matches one of the keywords exactly in the document" $ do
      let filter' = strictKeywords ["nonexistent", "Haskell programming"]
      filt filter' (testMetadata, strictKeywordsMarkdownAst) `shouldBe` True

    it "does not match when none of the exact keywords are present" $ do
      let filter' = strictKeywords ["Ruby", "Java", "C++"]
      filt filter' (testMetadata, strictKeywordsMarkdownAst) `shouldBe` False

    it "matches all exact keywords present in the document" $ do
      let filter' = strictKeywords ["strict search", "keyword detection"]
      filt filter' (testMetadata, strictKeywordsMarkdownAst) `shouldBe` True

    it "does not match partial keywords" $ do
      let filter' = strictKeywords ["stricted", "searching"]
      filt filter' (testMetadata, strictKeywordsMarkdownAst) `shouldBe` False

    it "matches exact keywords regardless of their location in the document" $ do
      let filter' = strictKeywords ["text analysis"]
      filt filter' (testMetadata, strictKeywordsMarkdownAst) `shouldBe` True

    it "matches exact phrases including special characters" $ do
      let filter' = strictKeyword "\"strict search\""
      filt filter' (testMetadata, strictKeywordsMarkdownAst) `shouldBe` True

    it "does not match keywords if not exactly present (case-sensitive)" $ do
      let filter' = strictKeywords ["Strict Search", "haskell Programming"]
      filt filter' (testMetadata, strictKeywordsMarkdownAst) `shouldBe` False

spec :: Spec
spec = do
  metadataSpec
  keywordsFilterSpec
  strictKeywordsFilterSpec
