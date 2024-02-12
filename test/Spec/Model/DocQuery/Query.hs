{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Model.DocQuery.Query (spec) where

import Commonmark
import Commonmark.Extensions
import Data.Either.Combinators
import Data.Functor.Identity
import qualified Data.Text as T
import Data.Time
import Model.DocQuery
import Model.Document
import Model.MarkdownAst hiding (Alert)
import qualified Model.Metadata as M
import Parser.Markdown
import qualified Path as P
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
sampleMarkdownAst = rightToMaybe $ markdownAst "test1" (T.pack sampleMarkdownDoc)

sampleMetadata :: M.Metadata
sampleMetadata =
  M.Metadata
    { M.title = Just "Test Title",
      M.author = Just "Test Author",
      M.dateTime = Just (parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-06-15T12:00:00Z"),
      M.tags = ["haskell", "parsing"],
      M.description = "This is a test document with some Haskell and parsing"
    }

samplePath :: P.Path P.Rel P.File
samplePath = $(P.mkRelFile "some/test/path/file.txt")

sampleDoc :: Document
sampleDoc =
  Document
    { relPath = samplePath,
      timeCreated = UTCTime (fromGregorian 2024 1 1) 0,
      lastModified = UTCTime (fromGregorian 2024 1 1) 0,
      filename = "sample.md",
      metadata = sampleMetadata,
      ast = sampleMarkdownAst,
      text = T.pack sampleMarkdownDoc
    }

metadataQuerySpec :: Spec
metadataQuerySpec = describe "Metadata Filter" $ do
  let doc = sampleDoc
  it "matches metadata with a specific date range" $ do
    let startDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-01-01T00:00:00Z"
    let endDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-12-31T23:59:59Z"
    let filter' = DateTimeRange (Just startDate) (Just endDate)
    query filter' doc `shouldBe` True

  it "does not match metadata outside of a specific date range" $ do
    let beforeDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2020-01-01T00:00:00Z"
    let afterDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-01-01T00:00:00Z"
    let filter' = DateTimeRange (Just beforeDate) (Just afterDate)
    query filter' doc `shouldBe` False

  it "matches metadata when only start date is given" $ do
    let startDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-01-01T00:00:00Z"
    let filter' = DateTimeRange (Just startDate) Nothing
    query filter' doc `shouldBe` True

  it "matches metadata when only end date is given" $ do
    let endDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" "2021-12-31T23:59:59Z"
    let filter' = DateTimeRange Nothing (Just endDate)
    query filter' doc `shouldBe` True

  it "matches metadata when no date range is given" $ do
    let filter' = DateTimeRange Nothing Nothing
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
    let doc = sampleDoc

    it "matches document with specific relative path" $ do
      query (InDirectory $(P.mkRelDir "some/test/path")) doc `shouldBe` True
      query (InDirectory $(P.mkRelDir "some/test")) doc `shouldBe` True

    it "does not match document with different relative path" $ do
      query (InDirectory $(P.mkRelDir "other/test/path")) doc `shouldBe` False

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

  let testMarkdownAstWithLink = rightToMaybe $ markdownAst "test1" (T.pack testDocWithLink)
  let testMarkdownAstWithoutLink = rightToMaybe $ markdownAst "test1" (T.pack testDocWithoutLink)
  let testMarkdownAstMultipleLinks = rightToMaybe $ markdownAst "test1" (T.pack testDocMultipleLinks)
  let docWithLink = sampleDoc {ast = testMarkdownAstWithLink, text = (T.pack testDocWithLink)}
  let docWithoutLink = sampleDoc {ast = testMarkdownAstWithoutLink, text = (T.pack testDocWithoutLink)}
  let multipleLinksDoc = sampleDoc {ast = testMarkdownAstMultipleLinks, text = (T.pack testDocMultipleLinks)}
  let linkInDoc = $(P.mkRelFile "someLink/here/test.md")
  let nonExistentLink = $(P.mkRelFile "nonexistent/link.md")

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
      let doc = sampleDoc
      let query' = Content (And (Val (StrictTerm "Markdown")) (Val (FuzzyTerm "iteem")))
      query query' doc `shouldBe` True

    it "does not match a document when one term in an AND query is not satisfied" $ do
      let doc = sampleDoc
      let query' = Content (And (Val (StrictTerm "markdown")) (Val (FuzzyTerm "test")))
      query query' doc `shouldBe` False

    it "matches a document with content that satisfies at least one term in an OR query" $ do
      let doc = sampleDoc
      let query' = Content (Or (Val (StrictTerm "markdown")) (Val (StrictTerm "Ordered")))
      query query' doc `shouldBe` True

    it "does not match a document when none of the terms in an OR query are satisfied" $ do
      let doc = sampleDoc
      let query' = Content (Or (Val (StrictTerm "markup")) (Val (StrictTerm "markdown")))
      query query' doc `shouldBe` False

taskQuerySpec :: Spec
taskQuerySpec = describe "Task Query Functionality" $ parallel $ do
  let sampleMarkdownDocWithTasks =
        [r|
# Introduction

This is a Markdown document with a variety of elements for **testing purposes**.

## Task List

- [x] Done task 1
- [ ] Todo task 2
- [x] Done task 3 with specific term
- [ ] Todo task 4 with general term
|]
  let sampleMarkdownAstWithTasks =
        rightToMaybe $
          runIdentity $
            markdownAstWith
              ( gfmExtensions <> defaultSyntaxSpec
              )
              "testWithTasks"
              (T.pack sampleMarkdownDocWithTasks)
  let docWithTasks =
        sampleDoc
          { ast = sampleMarkdownAstWithTasks,
            text = (T.pack sampleMarkdownDocWithTasks)
          }

  it "matches a document with 'Done' tasks containing a specific term" $ do
    let query' = Task Done (Val (StrictTerm "specific term"))
    query query' docWithTasks `shouldBe` True

  it "does not match a document if no 'Todo' tasks contain the specific term" $ do
    let query' = Task Todo (Val (StrictTerm "nonexistent term"))
    query query' docWithTasks `shouldBe` False

  it "matches a document with any tasks ('Both') containing a specific term" $ do
    let query' = Task Both (Val (FuzzyTerm "general term"))
    query query' docWithTasks `shouldBe` True

alertQuerySpec :: Spec
alertQuerySpec = describe "Alert Query Functionality" $ parallel $ do
  let sampleMarkdownDocWithAlerts =
        [r|
# Introduction

 This is a Markdown document with alerts for **testing purposes**.

 ## Alerts

 > [!WARNING]
 > Warning alert with a specific term.

 > [!IMPORTANT]
 > Important alert with another term.

 > [!NOTE]
 > Another alert
|]
  let sampleMarkdownAstWithAlerts =
        rightToMaybe $
          runIdentity $
            markdownAstWith
              ( gfmExtensions <> defaultSyntaxSpec
              )
              "testWithTasks"
              (T.pack sampleMarkdownDocWithAlerts)
  let docWithAlerts =
        sampleDoc
          { ast = sampleMarkdownAstWithAlerts,
            text = (T.pack sampleMarkdownDocWithAlerts)
          }

  it "matches a document with a specific type of alert containing a term" $ do
    let query' = Alert WarningAlert (Val (StrictTerm "term"))
    query query' docWithAlerts `shouldBe` True

  it "does not match a document with alerts of a different type" $ do
    let query' = Alert WarningAlert (Val (StrictTerm "another"))
    query query' docWithAlerts `shouldBe` False

  it "matches a document with alerts of a specific type using a complex boolean expression" $ do
    let query' = Alert NoteAlert (And (Val (FuzzyTerm "AlErT")) (Not (Val (StrictTerm "Alert"))))
    query query' docWithAlerts `shouldBe` True

complexQuerySpec :: Spec
complexQuerySpec = describe "Complex Query Functionality" $ parallel $ do
  let sampleMarkdownDocWithTasksAndAlerts =
        [r|
 # Document with Tasks and Alerts

 ## Tasks

 - [x] Completed task with specific term
 - [ ] Unfinished task
 - [x] Another completed task

 ## Alerts

 > [!IMPORTANT]
 > Important alert with a specific term.

 > [!WARNING]
 > Warning alert with general term.

 ## Content

|]

  let sampleMarkdownAstWithTasksAndAlerts =
        rightToMaybe $
          runIdentity $
            markdownAstWith
              ( gfmExtensions <> defaultSyntaxSpec
              )
              "testWithTasks"
              (T.pack sampleMarkdownDocWithTasksAndAlerts)
  let docWithTasksAndAlerts =
        sampleDoc
          { ast = sampleMarkdownAstWithTasksAndAlerts,
            text = (T.pack sampleMarkdownDocWithTasksAndAlerts)
          }

  it "matches a document with both a specific alert and a done task" $ do
    let alertQuery = Alert ImportantAlert (Val (CaseInsensitiveTerm "important"))
    let taskQuery = Task Done (Val (StrictTerm "specific"))
    let complexQuery = And (Val alertQuery) (Val taskQuery)
    query complexQuery docWithTasksAndAlerts `shouldBe` True

  it "does not match a document when only one part of an AND query is satisfied" $ do
    let alertQuery = Alert ImportantAlert (Val (StrictTerm "nonexistent"))
    let taskQuery = Task Done (Val (StrictTerm "specific"))
    let complexQuery = And (Val alertQuery) (Val taskQuery)
    query complexQuery docWithTasksAndAlerts `shouldBe` False

  it "matches a document when at least one part of an OR query is satisfied" $ do
    let alertQuery = Alert ImportantAlert (Val (FuzzyTerm "important"))
    let taskQuery = Task Done (Val (StrictTerm "nonexistent"))
    let complexQuery = Or (Val alertQuery) (Val taskQuery)
    query complexQuery docWithTasksAndAlerts `shouldBe` True

spec :: Spec
spec = parallel $ do
  metadataQuerySpec
  pathQuerySpec
  linkQuerySpec
  contentQuerySpec
  taskQuerySpec
  alertQuerySpec
  complexQuerySpec
