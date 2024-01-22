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

metadataSpec :: Spec
metadataSpec = describe "Metadata Filter" $ do
  let doc = Document samplePath sampleMetadata sampleMarkdownAst (T.pack sampleMarkdownDoc)
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
    let filter' = author $ strictTerm "Test Author"
    filt filter' doc `shouldBe` True

  it "does not match metadata with a different author" $ do
    let filter' = author $ strictTerm "Nonexistent Author"
    filt filter' doc `shouldBe` False

  it "matches metadata with the specific title" $ do
    let filter' = title $ strictTerm "Test Title"
    filt filter' doc `shouldBe` True

  it "does not match metadata with a different title" $ do
    let filter' = title $ strictTerm "Nonexistent Title"
    filt filter' doc `shouldBe` False

  it "matches metadata with a specific tag" $ do
    let filter' = tag $ strictTerm "haskell"
    filt filter' doc `shouldBe` True

  it "does not match metadata without a specific tag" $ do
    let filter' = tag $ strictTerm "nonexistent"
    filt filter' doc `shouldBe` False

  it "matches metadata with fuzzy description" $ do
    let filter' = description (fuzzyTerm "Haskell and * parsing")
    filt filter' doc `shouldBe` True

  it "matches metadata that does not contain the fuzzy description" $ do
    let filter' = description (fuzzyTerm "unrelated topic")
    filt filter' doc `shouldBe` False

  it "matches metadata when fuzzy description is partially matched" $ do
    let filter' = description (fuzzyTerm "test document")
    filt filter' doc `shouldBe` True

  it "does not match metadata when fuzzy description is not matched at all" $ do
    let filter' = description (fuzzyTerm "completely unrelated")
    filt filter' doc `shouldBe` False

  it "matches metadata with case insensitive fuzzy description" $ do
    let filter' = description (fuzzyTerm "HASKELL AND PARSING")
    filt filter' doc `shouldBe` True

fuzzyTermSpec :: Spec
fuzzyTermSpec = describe "fuzzyTerm Filter" $ do
  let doc =
        T.pack
          [r|# Sample Markdown Document

This document is for testing the keyword search functionality.

It contains some *keywords* that should be detected by the filter.

Here are some of the keywords we might want to find:
- Haskell
- Parsing
- Keyword
- Search
|]
  it "matches a single term present in the document" $ do
    filt (fuzzyTerm "Haskell") doc `shouldBe` True

  it "does not match a term that is not present in the document" $ do
    filt (fuzzyTerm "Nonexistent") doc `shouldBe` False

  it "matches keywords with mixed case in the document" $ do
    filt (fuzzyTerm "haskell") doc `shouldBe` True
    filt (fuzzyTerm "parsing") doc `shouldBe` True

  it "matches partial keywords if they are part of a word in the document" $ do
    filt (fuzzyTerm "key") doc `shouldBe` True
    filt (fuzzyTerm "pars") doc `shouldBe` True

strictTermFilterSpec :: Spec
strictTermFilterSpec = describe "Strict Keywords Filter" $ do
  let doc =
        T.pack
          [r|
# Strict Keyword Search Document

This document is specifically for testing strict keyword search.

It contains exact phrases like "strict search" and "keyword detection".

Other phrases include "Haskell programming" and "text analysis".

|]
  it "matches an exact keyword in the document" $ do
    filt (strictTerm "strict search") doc `shouldBe` True

  it "does not match a keyword if not an exact match" $ do
    filt (strictTerm "nonexist") doc `shouldBe` False

  it "does not match partial keywords" $ do
    filt (strictTerm "stricted") doc `shouldBe` False
    filt (strictTerm "searching") doc `shouldBe` False

  it "matches exact keywords regardless of their location in the document" $ do
    filt (strictTerm "text analysis") doc `shouldBe` True

  it "matches exact phrases including special characters" $ do
    filt (strictTerm "\"strict search\"") doc `shouldBe` True

  it "does not match keywords if not exactly present (case-sensitive)" $ do
    filt (strictTerm "Strict Search") doc `shouldBe` False
    filt (strictTerm "haskell Programming") doc `shouldBe` False

regexFilterSpec :: Spec
regexFilterSpec = do
  describe "Regex Matching Filter" $ do
    let posixRegexMatchDoc =
          T.pack
            [r|
# POSIX Regex Matching Document

This document contains various POSIX extended regular expressions for testing.

1. Haskell 101 - An introductory course.
2. The world of Haskell programming.
3. Learn Haskell 202, the next step in functional programming.
4. The word 'Haskell' appears multiple times in this document.
5. This is a sample phone number: 123-456-7890.
6. Email address: user@example.com
|]

    let nonPosixRegexMatchDoc =
          T.pack
            [r|
# Non-POSIX Regex Matching Document

This document does not contain specific POSIX extended regular expressions.

1. Introduction to functional programming.
2. Exploring different programming paradigms.
3. A comprehensive study of programming languages.
4. This is not a valid phone number: 1234567.
5. This is not a valid email address: userexample.com
|]

    it "matches document containing text that matches a POSIX-compatible regex" $ do
      filt (regexTerm "Haskell [0-9]+") posixRegexMatchDoc `shouldBe` True

    it "does not match document without text that matches the POSIX-compatible regex" $ do
      filt (regexTerm "Haskell [0-9]+") nonPosixRegexMatchDoc `shouldBe` False

    it "matches document when POSIX-compatible regex matches anywhere in the text" $ do
      filt (regexTerm "world") posixRegexMatchDoc `shouldBe` True

    it "does not match document when POSIX-compatible regex does not match any part of the text" $ do
      filt (regexTerm "^nonexistent") nonPosixRegexMatchDoc `shouldBe` False

    it "matches document containing text that matches a phone number regex" $ do
      filt (regexTerm "[0-9]{3}-[0-9]{3}-[0-9]{4}") posixRegexMatchDoc `shouldBe` True

    it "matches document containing text that matches an email address regex" $ do
      filt (regexTerm "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}") posixRegexMatchDoc `shouldBe` True

pathFilterSpec :: Spec
pathFilterSpec = do
  describe "Path Matching Filter" $ do
    let doc = Document samplePath sampleMetadata sampleMarkdownAst (T.pack sampleMarkdownDoc)

    it "matches document with specific relative path" $ do
      let filter' = matchesRelPath $(mkRelFile "some/test/path/file.txt")
      filt filter' doc `shouldBe` True

    it "does not match document with different relative path" $ do
      let filter' = matchesRelPath $(mkRelFile "other/test/path/file.txt")
      filt filter' doc `shouldBe` False

linkFilterSpec :: Spec
linkFilterSpec = do
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

  describe "Has Link Filter" $ do
    let docWithLink = Document samplePath sampleMetadata testMarkdownAstWithLink (T.pack testDocWithLink)
    let docWithoutLink = Document samplePath sampleMetadata testMarkdownAstWithoutLink (T.pack testDocWithoutLink)

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
      let multipleLinksDoc = Document samplePath sampleMetadata testMarkdownAstMultipleLinks (T.pack testDocMultipleLinks)
      let filter' = hasLink linkInDoc
      filt filter' multipleLinksDoc `shouldBe` True

spec :: Spec
spec = do
  metadataSpec
  fuzzyTermSpec
  strictTermFilterSpec
  regexFilterSpec
  pathFilterSpec
  linkFilterSpec
