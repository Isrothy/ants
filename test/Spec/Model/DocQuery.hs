{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Model.DocQuery (spec) where

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

metadataSpec :: Spec
metadataSpec = describe "Metadata Filter" $ do
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
    let filter' = Author $ StrictTerm "Test Author"
    query filter' doc `shouldBe` True

  it "does not match metadata with a different author" $ do
    let filter' = Author $ StrictTerm "Nonexistent Author"
    query filter' doc `shouldBe` False

  it "matches metadata with the specific title" $ do
    let filter' = Title $ StrictTerm "Test Title"
    query filter' doc `shouldBe` True

  it "does not match metadata with a different title" $ do
    let filter' = Title $ StrictTerm "Nonexistent Title"
    query filter' doc `shouldBe` False

  it "matches metadata with a specific tag" $ do
    let filter' = Tag $ StrictTerm "haskell"
    query filter' doc `shouldBe` True

  it "does not match metadata without a specific tag" $ do
    let filter' = Tag $ StrictTerm "nonexistent"
    query filter' doc `shouldBe` False

  it "matches metadata with fuzzy description" $ do
    let filter' = Description (FuzzyTerm "Haskell and * parsing")
    query filter' doc `shouldBe` True

  it "matches metadata that does not contain the fuzzy description" $ do
    let filter' = Description (FuzzyTerm "unrelated topic")
    query filter' doc `shouldBe` False

  it "matches metadata when fuzzy description is partially matched" $ do
    let filter' = Description (FuzzyTerm "test document")
    query filter' doc `shouldBe` True

  it "does not match metadata when fuzzy description is not matched at all" $ do
    let filter' = Description (FuzzyTerm "completely unrelated")
    query filter' doc `shouldBe` False

  it "matches metadata with case insensitive fuzzy description" $ do
    let filter' = Description (FuzzyTerm "HASKELL AND PARSING")
    query filter' doc `shouldBe` True

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
    match (FuzzyTerm "Haskell") doc `shouldBe` True

  it "does not match a term that is not present in the document" $ do
    match (FuzzyTerm "Nonexistent") doc `shouldBe` False

  it "matches keywords with mixed case in the document" $ do
    match (FuzzyTerm "haskell") doc `shouldBe` True
    match (FuzzyTerm "parsing") doc `shouldBe` True

  it "matches partial keywords if they are part of a word in the document" $ do
    match (FuzzyTerm "key") doc `shouldBe` True
    match (FuzzyTerm "pars") doc `shouldBe` True

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
    match (StrictTerm "strict search") doc `shouldBe` True

  it "does not match a keyword if not an exact match" $ do
    match (StrictTerm "nonexist") doc `shouldBe` False

  it "does not match partial keywords" $ do
    match (StrictTerm "stricted") doc `shouldBe` False
    match (StrictTerm "searching") doc `shouldBe` False

  it "matches exact keywords regardless of their location in the document" $ do
    match (StrictTerm "text analysis") doc `shouldBe` True

  it "matches exact phrases including special characters" $ do
    match (StrictTerm "\"strict search\"") doc `shouldBe` True

  it "does not match keywords if not exactly present (case-sensitive)" $ do
    match (StrictTerm "Strict Search") doc `shouldBe` False
    match (StrictTerm "haskell Programming") doc `shouldBe` False

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
      match (RegexTerm "Haskell [0-9]+") posixRegexMatchDoc `shouldBe` True

    it "does not match document without text that matches the POSIX-compatible regex" $ do
      match (RegexTerm "Haskell [0-9]+") nonPosixRegexMatchDoc `shouldBe` False

    it "matches document when POSIX-compatible regex matches anywhere in the text" $ do
      match (RegexTerm "world") posixRegexMatchDoc `shouldBe` True

    it "does not match document when POSIX-compatible regex does not match any part of the text" $ do
      match (RegexTerm "^nonexistent") nonPosixRegexMatchDoc `shouldBe` False

    it "matches document containing text that matches a phone number regex" $ do
      match (RegexTerm "[0-9]{3}-[0-9]{3}-[0-9]{4}") posixRegexMatchDoc `shouldBe` True

    it "matches document containing text that matches an email address regex" $ do
      match (RegexTerm "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}") posixRegexMatchDoc `shouldBe` True

pathFilterSpec :: Spec
pathFilterSpec = do
  describe "Path Matching Filter" $ do
    let doc = Document samplePath sampleMetadata sampleMarkdownAst (T.pack sampleMarkdownDoc)

    it "matches document with specific relative path" $ do
      query (InDirectory $(mkRelDir "some/test/path")) doc `shouldBe` True
      query (InDirectory $(mkRelDir "some/test")) doc `shouldBe` True

    it "does not match document with different relative path" $ do
      query (InDirectory $(mkRelDir "other/test/path")) doc `shouldBe` False

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

spec :: Spec
spec = do
  metadataSpec
  fuzzyTermSpec
  strictTermFilterSpec
  regexFilterSpec
  pathFilterSpec
  linkFilterSpec
