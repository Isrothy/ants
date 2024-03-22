{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Parser.DocQuery
  ( spec,
  )
where

import Commonmark.Extensions (AlertType (..))
import Data.Either
import Data.Time
import Model.DocQuery
import Parser.DocQuery
import Test.Hspec
import Text.Parsec
import Text.Parsec.Text (Parser)

boolTerm :: Parser (BoolExpr Term)
boolTerm = boolExpr

searchTermSpec :: Spec
searchTermSpec = describe "SearchTermParser" $ parallel $ do
  describe "Basic term parsing" $ do
    it "parses a quotesearchTerm term" $ do
      parse doubleQuotedTerm "" "\"example\"" `shouldBe` Right (CaseInsensitiveTerm "example")

    it "parses an unquoted term" $ do
      parse term "" "example" `shouldBe` Right (CaseInsensitiveTerm "example")

  describe "Strict term parsing" $ do
    it "parses a strictTerm term" $ do
      parse singleQuotedTerm "" "\'example\'" `shouldBe` Right (StrictTerm "example")

  describe "Regex term parsing" $ do
    it "parses a regex term" $ do
      parse regexTerm "" "/[a-z]+/" `shouldBe` Right (RegexTerm "[a-z]+")

  describe "Fuzzy term parsing" $ do
    it "parses a fuzzy term" $ do
      parse fuzzyTerm "" "~example~" `shouldBe` Right (FuzzyTerm "example")

booleanTermSpec :: Spec
booleanTermSpec = describe "BoolenTermParser" $ parallel $ do
  describe "Boolean operations parsing" $ parallel $ do
    it "parses an OR operation" $ do
      parse boolTerm "" "term1 || term2" `shouldBe` Right (Var (CaseInsensitiveTerm "term1") `Or` Var (CaseInsensitiveTerm "term2"))
      parse boolTerm "" "term1||  term2" `shouldBe` Right (Var (CaseInsensitiveTerm "term1") `Or` Var (CaseInsensitiveTerm "term2"))
      parse boolTerm "" "term1 ||term2" `shouldBe` Right (Var (CaseInsensitiveTerm "term1") `Or` Var (CaseInsensitiveTerm "term2"))
      parse boolTerm "" "term1||term2" `shouldBe` Right (Var (CaseInsensitiveTerm "term1") `Or` Var (CaseInsensitiveTerm "term2"))

    it "parses an AND operation" $ do
      parse boolTerm "" "/term1/ && term2" `shouldBe` Right (Var (RegexTerm "term1") `And` Var (CaseInsensitiveTerm "term2"))
      parse boolTerm "" "/term1/&& term2" `shouldBe` Right (Var (RegexTerm "term1") `And` Var (CaseInsensitiveTerm "term2"))
      parse boolTerm "" "/term1/   &&term2" `shouldBe` Right (Var (RegexTerm "term1") `And` Var (CaseInsensitiveTerm "term2"))
      parse boolTerm "" "/term1/&&term2" `shouldBe` Right (Var (RegexTerm "term1") `And` Var (CaseInsensitiveTerm "term2"))

    it "parses a NOT operation" $ do
      parse boolTerm "" "!term1" `shouldBe` Right (Not (Var (CaseInsensitiveTerm "term1")))
      parse boolTerm "" "! term1" `shouldBe` Right (Not (Var (CaseInsensitiveTerm "term1")))
      parse boolTerm "" "!   term1" `shouldBe` Right (Not (Var (CaseInsensitiveTerm "term1")))

  describe "Complex expression parsing" $ parallel $ do
    it "parses complex expressions" $ do
      parse boolTerm "" "term1 && (term2 || !term3)"
        `shouldBe` Right
          (Var (CaseInsensitiveTerm "term1") `And` (Var (CaseInsensitiveTerm "term2") `Or` Not (Var (CaseInsensitiveTerm "term3"))))
      parse boolTerm "" "~fuzzy~ && /[a-z]+/ && \"exact\""
        `shouldBe` Right (Var (FuzzyTerm "fuzzy") `And` Var (RegexTerm "[a-z]+") `And` Var (CaseInsensitiveTerm "exact"))
    it "parses combinations of different operations and terms" $ do
      parse boolTerm "" "(term1)" `shouldBe` Right (Var (CaseInsensitiveTerm "term1"))
      parse boolTerm "" "((term1))" `shouldBe` Right (Var (CaseInsensitiveTerm "term1"))
      parse boolTerm "" "((  (term1 )) )" `shouldBe` Right (Var (CaseInsensitiveTerm "term1"))
      parse boolTerm "" "(term1 || !term2)  && ~fuzzyTerm~ && /regexTerm/"
        `shouldBe` Right
          ( (Var (CaseInsensitiveTerm "term1") `Or` Not (Var (CaseInsensitiveTerm "term2")))
              `And` Var (FuzzyTerm "fuzzyTerm")
              `And` Var (RegexTerm "regexTerm")
          )
      parse boolTerm "" "term1 || term2 || (term3)"
        `shouldBe` Right
          (Var (CaseInsensitiveTerm "term1") `Or` Var (CaseInsensitiveTerm "term2") `Or` Var (CaseInsensitiveTerm "term3"))
      parse boolTerm "" "!term1  && !term2"
        `shouldBe` Right
          (Not (Var (CaseInsensitiveTerm "term1")) `And` Not (Var (CaseInsensitiveTerm "term2")))
      parse boolTerm "" "term1 || ! term2 || ( term3 && !(term4))"
        `shouldBe` Right
          ( Var (CaseInsensitiveTerm "term1")
              `Or` Not (Var (CaseInsensitiveTerm "term2"))
              `Or` (Var (CaseInsensitiveTerm "term3") `And` Not (Var (CaseInsensitiveTerm "term4")))
          )
      parse boolTerm "" "!(  term1 || !term2)"
        `shouldBe` Right (Not (Var (CaseInsensitiveTerm "term1") `Or` Not (Var (CaseInsensitiveTerm "term2"))))

  describe "Multiple NOT operations" $ parallel $ do
    it "fails to parse double NOT operations (e.g., !!)" $ do
      parse boolTerm "" "!!term1" `shouldBe` Right (Not (Not (Var (CaseInsensitiveTerm "term1"))))
      parse boolTerm "" "! !term1" `shouldBe` Right (Not (Not (Var (CaseInsensitiveTerm "term1"))))
      parse boolTerm "" "!! term1" `shouldBe` Right (Not (Not (Var (CaseInsensitiveTerm "term1"))))
      parse boolTerm "" "! !  term1" `shouldBe` Right (Not (Not (Var (CaseInsensitiveTerm "term1"))))
    it "fails to parse triple NOT operations (e.g., !!!)" $ do
      parse boolTerm "" "!!!term1" `shouldBe` Right (Not (Not (Not (Var (CaseInsensitiveTerm "term1")))))
      parse boolTerm "" "!!! term1" `shouldBe` Right (Not (Not (Not (Var (CaseInsensitiveTerm "term1")))))
      parse boolTerm "" "! !!term1" `shouldBe` Right (Not (Not (Not (Var (CaseInsensitiveTerm "term1")))))
    it "fails to parse multiple consecutive NOT operations (e.g., !!)" $ do
      parse boolTerm "" "!! term1 && ! !term2"
        `shouldBe` Right (Not (Not (Var (CaseInsensitiveTerm "term1"))) `And` Not (Not (Var (CaseInsensitiveTerm "term2"))))
      parse boolTerm "" "!!term1 && ! ! term2 && ! !term3"
        `shouldBe` Right
          ( Not (Not (Var (CaseInsensitiveTerm "term1")))
              `And` Not (Not (Var (CaseInsensitiveTerm "term2")))
              `And` Not (Not (Var (CaseInsensitiveTerm "term3")))
          )

  describe "Edge case handling" $ do
    it "handles an empty input" $ do
      parse boolTerm "" "" `shouldSatisfy` isLeft

searchQuerySpec :: Spec
searchQuerySpec = describe "SearchTermParser" $ parallel $ do
  describe "Author Query Parser" $ do
    it "parses an author query" $ do
      parse author "" "author:\"John Doe\"" `shouldBe` Right (Author (Var (CaseInsensitiveTerm "John Doe")))
      parse author "" "author:John" `shouldBe` Right (Author (Var (CaseInsensitiveTerm "John")))

    it "parses an author query with a complex boolean expression" $ do
      let input = "author:(\"John Doe\" || ~Jane~)"
      parse author "" input `shouldBe` Right (Author (Or (Var (CaseInsensitiveTerm "John Doe")) (Var (FuzzyTerm "Jane"))))

  describe "Tag Query Parser" $ parallel $ do
    it "parses a tag query" $ do
      parse tag "" "tag:\"Haskell\"" `shouldBe` Right (Tag (Var (CaseInsensitiveTerm "Haskell")))
      parse tag "" "tag:\'Haskell\'" `shouldBe` Right (Tag (Var (StrictTerm "Haskell")))

    it "parses a tag query with a NOT expression" $ do
      let input = "tag:(!\"Haskell\")"
      parse tag "" input `shouldBe` Right (Tag (Not (Var (CaseInsensitiveTerm "Haskell"))))

    it "parses a tag query with an AND expression" $ do
      let input = "tag:(\"Haskell\" && 'functional')"
      parse tag "" input `shouldBe` Right (Tag (And (Var (CaseInsensitiveTerm "Haskell")) (Var (StrictTerm "functional"))))

  describe "Description Query Parser" $ do
    it "parses a description query" $ do
      parse description "" "description:\"Haskell programming\"" `shouldBe` Right (Description (Var (CaseInsensitiveTerm "Haskell programming")))
      parse description "" "description:~Haskell programming~" `shouldBe` Right (Description (Var (FuzzyTerm "Haskell programming")))

  describe "HasLink Query Parser" $ do
    it "parses a simple hasLink query" $ do
      parse hasLink "" "has-link:this/is/a/link" `shouldBe` Right (HasLink "this/is/a/link")

    it "parses a hasLink query with a complex path" $ do
      parse hasLink "" "has-link:this/is/a/deep/link/path" `shouldBe` Right (HasLink "this/is/a/deep/link/path")

    it "parses a hasLink query with special characters" $ do
      parse hasLink "" "has-link:this/link-has_special.chars?query=1#section" `shouldBe` Right (HasLink "this/link-has_special.chars?query=1#section")

    it "parses a hasLink query with numeric and dashed path" $ do
      parse hasLink "" "has-link:2021/03/14/article-name" `shouldBe` Right (HasLink "2021/03/14/article-name")

    it "fails to parse a hasLink query without a link" $ do
      parse hasLink "" "has-link:" `shouldSatisfy` isLeft

    it "fails to parse an incorrect hasLink query" $ do
      parse hasLink "" "has-link" `shouldSatisfy` isLeft

  describe "Content Query Parser" $ parallel $ do
    it "parses a content query" $ do
      parse content "" "content:\"sample content\"" `shouldBe` Right (Content (Var (CaseInsensitiveTerm "sample content")))
      parse content "" "content:/sample content/" `shouldBe` Right (Content (Var (RegexTerm "sample content")))

    it "parses a content query with AND combination of strict and fuzzy terms" $ do
      let input = "content:(\"term1\" && ~fuzzy~)"
      parse content "" input `shouldBe` Right (Content (And (Var (CaseInsensitiveTerm "term1")) (Var (FuzzyTerm "fuzzy"))))

    it "parses a content query with OR combination of strict and case-insensitive terms" $ do
      let input = "content:(\"term1\" || 'term2')"
      parse content "" input `shouldBe` Right (Content (Or (Var (CaseInsensitiveTerm "term1")) (Var (StrictTerm "term2"))))

    it "parses a content query with nested boolean expressions" $ do
      let input = "content:(\"term1\" && (~fuzzy~ || 'term2'))"
      parse content "" input `shouldBe` Right (Content (And (Var (CaseInsensitiveTerm "term1")) (Or (Var (FuzzyTerm "fuzzy")) (Var (StrictTerm "term2")))))

  describe "Task Query Passer" $ parallel $ do
    it "parses a task query with Done tasks and a strict term" $ do
      let input = "task-done:\"exact term\""
      parse task "" input `shouldBe` Right (Task Done (Var (CaseInsensitiveTerm "exact term")))

    it "parses a task query with Todo tasks and a fuzzy term" $ do
      let input = "task-todo:~fuzzyTerm~"
      parse task "" input `shouldBe` Right (Task Todo (Var (FuzzyTerm "fuzzyTerm")))

    it "parses a task query with Both tasks and a regex term" $ do
      let input = "task:/regexTerm/"
      parse task "" input `shouldBe` Right (Task Both (Var (RegexTerm "regexTerm")))

    it "parses a task query with Done tasks and a case insensitive term" $ do
      let input = "task-done:\"Case Term\""
      parse task "" input `shouldBe` Right (Task Done (Var (CaseInsensitiveTerm "Case Term")))

    it "parses a task query with Todo tasks and a compound boolean expression" $ do
      let input = "task-todo:(\'term1\' || ~term2~)"
      parse task "" input `shouldBe` Right (Task Todo (Or (Var (StrictTerm "term1")) (Var (FuzzyTerm "term2"))))

    it "fails to parse an invalid task query" $ do
      let input = "task-unknown:\"term\""
      parse task "" input `shouldSatisfy` isLeft

  describe "Alert Query Parser" $ parallel $ do
    it "parses an alert query with NoteAlert type and a strict term" $ do
      let input = "alert-note:\'exact term\'"
      parse alert "" input `shouldBe` Right (Alert NoteAlert (Var (StrictTerm "exact term")))

    it "parses an alert query with TipAlert type and a fuzzy term" $ do
      let input = "alert-tip:~fuzzyTerm~"
      parse alert "" input `shouldBe` Right (Alert TipAlert (Var (FuzzyTerm "fuzzyTerm")))

    it "parses an alert query with ImportantAlert type and a regex term" $ do
      let input = "alert-important:/regexTerm/"
      parse alert "" input `shouldBe` Right (Alert ImportantAlert (Var (RegexTerm "regexTerm")))

    it "parses an alert query with WarningAlert type and a case insensitive term" $ do
      let input = "alert-warning:\"Case Term\""
      parse alert "" input `shouldBe` Right (Alert WarningAlert (Var (CaseInsensitiveTerm "Case Term")))

    it "parses an alert query with CautionAlert type and a compound boolean expression" $ do
      let input = "alert-caution:(\'term1\' || ~term2~)"
      parse alert "" input `shouldBe` Right (Alert CautionAlert (Or (Var (StrictTerm "term1")) (Var (FuzzyTerm "term2"))))

    it "fails to parse an invalid alert query" $ do
      let input = "alert-unknown:\"term\""
      parse alert "" input `shouldSatisfy` isLeft

  describe "DateTimeRange Query Parser" $ parallel $ do
    it "parses a date query" $ do
      let input = "date:2021-01-01"
      parse date "" input `shouldBe` Right (DateTimeRange (Just (UTCTime (fromGregorian 2021 1 1) 0)) (Just (UTCTime (fromGregorian 2021 1 2) 0)))

    it "parses a date range query with both start and end dates" $ do
      let input = "date:[2021-01-01,2021-12-31]"
      parse date "" input `shouldBe` Right (DateTimeRange (Just (UTCTime (fromGregorian 2021 1 1) 0)) (Just (UTCTime (fromGregorian 2022 1 1) 0)))

    it "parses a date range query with only a start date" $ do
      let input = "date:[2021-01-01,]"
      parse date "" input `shouldBe` Right (DateTimeRange (Just (UTCTime (fromGregorian 2021 1 1) 0)) Nothing)

    it "parses a date range query with only an end date" $ do
      let input = "date:[,2021-12-31]"
      parse date "" input `shouldBe` Right (DateTimeRange Nothing (Just (UTCTime (fromGregorian 2022 1 1) 0)))

    it "parses a date range query without dates" $ do
      let input = "date:[,]"
      parse date "" input `shouldBe` Right (DateTimeRange Nothing Nothing)

    it "fails to parse an invalid date range query" $ do
      let input = "date:[invalid,2021-12-31]"
      parse date "" input `shouldSatisfy` isLeft

completeQuerySpec :: Spec
completeQuerySpec = describe "Complete Query Parser" $ parallel $ do
  it "parses a single term query" $ do
    parse completeQuery "" "author:\"John Doe\""
      `shouldBe` Right
        (Var $ Author $ Var $ CaseInsensitiveTerm "John Doe")

  it "parses an AND combination query" $ do
    parse completeQuery "" "tag:\"Haskell\" && description:~functional~"
      `shouldBe` Right
        ( And
            (Var $ Tag $ Var $ CaseInsensitiveTerm "Haskell")
            (Var $ Description $ Var $ FuzzyTerm "functional")
        )

  it "parses an OR combination query" $ do
    parse completeQuery "" "content:\"algorithm\" || alert-tip:~efficiency~"
      `shouldBe` Right
        ( Or
            (Var $ Content $ Var $ CaseInsensitiveTerm "algorithm")
            (Var $ Alert TipAlert $ Var $ FuzzyTerm "efficiency")
        )

  it "parses nested boolean expressions" $ do
    parse completeQuery "" "author:\"Jane Doe\" && (tag:\"Haskell\" || content:~guide~)"
      `shouldBe` Right
        ( And
            (Var $ Author $ Var $ CaseInsensitiveTerm "Jane Doe")
            ( Or
                (Var $ Tag $ Var $ CaseInsensitiveTerm "Haskell")
                (Var $ Content $ Var $ FuzzyTerm "guide")
            )
        )

  it "parses a task query with boolean expression" $ do
    parse completeQuery "" "task-done:(\"Documentation\" || \'Haskell\') && task-todo:~testing~"
      `shouldBe` Right
        ( And
            ( Var $
                Task Done $
                  Or
                    (Var $ CaseInsensitiveTerm "Documentation")
                    (Var $ StrictTerm "Haskell")
            )
            (Var $ Task Todo $ Var $ FuzzyTerm "testing")
        )

  it "parses complex combined queries" $ do
    parse completeQuery "" "!author:\"John Doe\" && (content:~guide~ || (date:[2021-01-01,2021-12-31] && tag:\"Haskell\"))"
      `shouldBe` Right
        ( And
            (Not $ Var $ Author $ Var $ CaseInsensitiveTerm "John Doe")
            ( Or
                (Var $ Content $ Var $ FuzzyTerm "guide")
                ( And
                    ( Var $
                        DateTimeRange
                          (Just (UTCTime (fromGregorian 2021 1 1) 0))
                          (Just (UTCTime (fromGregorian 2022 1 1) 0))
                    )
                    (Var $ Tag $ Var $ CaseInsensitiveTerm "Haskell")
                )
            )
        )

  it "parses not operator queries" $ do
    parse completeQuery "" "!author:\"John Doe\""
      `shouldBe` Right
        (Not $ Var $ Author $ Var $ CaseInsensitiveTerm "John Doe")

  it "fails to parse incorrect syntax" $ do
    parse completeQuery "" "author:\"John Doe\" &&" `shouldSatisfy` isLeft

  it "fails to parse empty query" $ do
    parse completeQuery "" "" `shouldSatisfy` isLeft

spec :: Spec
spec = describe "SearchLanguageParser" $ parallel $ do
  searchTermSpec
  booleanTermSpec
  searchQuerySpec
  completeQuerySpec
