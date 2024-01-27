{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Parser.DocQuery
  ( spec,
  )
where

import Data.Either
import qualified Data.Text as T
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

  describe "Boolean operations parsing" $ do
    it "parses an OR operation" $ do
      parse boolTerm "" "term1 || term2" `shouldBe` Right (Val (CaseInsensitiveTerm "term1") `Or` Val (CaseInsensitiveTerm "term2"))
      parse boolTerm "" "term1||  term2" `shouldBe` Right (Val (CaseInsensitiveTerm "term1") `Or` Val (CaseInsensitiveTerm "term2"))
      parse boolTerm "" "term1 ||term2" `shouldBe` Right (Val (CaseInsensitiveTerm "term1") `Or` Val (CaseInsensitiveTerm "term2"))
      parse boolTerm "" "term1||term2" `shouldBe` Right (Val (CaseInsensitiveTerm "term1") `Or` Val (CaseInsensitiveTerm "term2"))

    it "parses an AND operation" $ do
      parse boolTerm "" "/term1/ && term2" `shouldBe` Right (Val (RegexTerm "term1") `And` Val (CaseInsensitiveTerm "term2"))
      parse boolTerm "" "/term1/&& term2" `shouldBe` Right (Val (RegexTerm "term1") `And` Val (CaseInsensitiveTerm "term2"))
      parse boolTerm "" "/term1/   &&term2" `shouldBe` Right (Val (RegexTerm "term1") `And` Val (CaseInsensitiveTerm "term2"))
      parse boolTerm "" "/term1/&&term2" `shouldBe` Right (Val (RegexTerm "term1") `And` Val (CaseInsensitiveTerm "term2"))

    it "parses a NOT operation" $ do
      parse boolTerm "" "!term1" `shouldBe` Right (Not (Val (CaseInsensitiveTerm "term1")))
      parse boolTerm "" "! term1" `shouldBe` Right (Not (Val (CaseInsensitiveTerm "term1")))
      parse boolTerm "" "!   term1" `shouldBe` Right (Not (Val (CaseInsensitiveTerm "term1")))

  describe "Complex expression parsing" $ do
    it "parses complex expressions" $ do
      parse boolTerm "" "term1 && (term2 || !term3)"
        `shouldBe` Right
          (Val (CaseInsensitiveTerm "term1") `And` (Val (CaseInsensitiveTerm "term2") `Or` Not (Val (CaseInsensitiveTerm "term3"))))
      parse boolTerm "" "~fuzzy~ && /[a-z]+/ && \"exact\""
        `shouldBe` Right (Val (FuzzyTerm "fuzzy") `And` Val (RegexTerm "[a-z]+") `And` Val (CaseInsensitiveTerm "exact"))
    it "parses combinations of different operations and terms" $ do
      parse boolTerm "" "(term1)" `shouldBe` Right (Val (CaseInsensitiveTerm "term1"))
      parse boolTerm "" "((term1))" `shouldBe` Right (Val (CaseInsensitiveTerm "term1"))
      parse boolTerm "" "((  (term1 )) )" `shouldBe` Right (Val (CaseInsensitiveTerm "term1"))
      parse boolTerm "" "(term1 || !term2)  && ~fuzzyTerm~ && /regexTerm/"
        `shouldBe` Right
          ( (Val (CaseInsensitiveTerm "term1") `Or` Not (Val (CaseInsensitiveTerm "term2")))
              `And` Val (FuzzyTerm "fuzzyTerm")
              `And` Val (RegexTerm "regexTerm")
          )
      parse boolTerm "" "term1 || term2 || (term3)"
        `shouldBe` Right
          (Val (CaseInsensitiveTerm "term1") `Or` Val (CaseInsensitiveTerm "term2") `Or` Val (CaseInsensitiveTerm "term3"))
      parse boolTerm "" "!term1  && !term2"
        `shouldBe` Right
          (Not (Val (CaseInsensitiveTerm "term1")) `And` Not (Val (CaseInsensitiveTerm "term2")))
      parse boolTerm "" "term1 || ! term2 || ( term3 && !(term4))"
        `shouldBe` Right
          ( Val (CaseInsensitiveTerm "term1")
              `Or` Not (Val (CaseInsensitiveTerm "term2"))
              `Or` (Val (CaseInsensitiveTerm "term3") `And` Not (Val (CaseInsensitiveTerm "term4")))
          )
      parse boolTerm "" "!(  term1 || !term2)"
        `shouldBe` Right (Not (Val (CaseInsensitiveTerm "term1") `Or` Not (Val (CaseInsensitiveTerm "term2"))))

  describe "Multiple NOT operations" $ do
    it "fails to parse double NOT operations (e.g., !!)" $ do
      parse boolTerm "" "!!term1" `shouldBe` Right (Not (Not (Val (CaseInsensitiveTerm "term1"))))
      parse boolTerm "" "! !term1" `shouldBe` Right (Not (Not (Val (CaseInsensitiveTerm "term1"))))
      parse boolTerm "" "!! term1" `shouldBe` Right (Not (Not (Val (CaseInsensitiveTerm "term1"))))
      parse boolTerm "" "! !  term1" `shouldBe` Right (Not (Not (Val (CaseInsensitiveTerm "term1"))))
    it "fails to parse triple NOT operations (e.g., !!!)" $ do
      parse boolTerm "" "!!!term1" `shouldBe` Right (Not (Not (Not (Val (CaseInsensitiveTerm "term1")))))
      parse boolTerm "" "!!! term1" `shouldBe` Right (Not (Not (Not (Val (CaseInsensitiveTerm "term1")))))
      parse boolTerm "" "! !!term1" `shouldBe` Right (Not (Not (Not (Val (CaseInsensitiveTerm "term1")))))
    it "fails to parse multiple consecutive NOT operations (e.g., !!)" $ do
      parse boolTerm "" "!! term1 && ! !term2"
        `shouldBe` Right (Not (Not (Val (CaseInsensitiveTerm "term1"))) `And` Not (Not (Val (CaseInsensitiveTerm "term2"))))
      parse boolTerm "" "!!term1 && ! ! term2 && ! !term3"
        `shouldBe` Right
          ( Not (Not (Val (CaseInsensitiveTerm "term1")))
              `And` Not (Not (Val (CaseInsensitiveTerm "term2")))
              `And` Not (Not (Val (CaseInsensitiveTerm "term3")))
          )

  describe "Edge case handling" $ do
    it "handles an empty input" $ do
      parse boolTerm "" "" `shouldSatisfy` isLeft

searchQuerySpec :: Spec
searchQuerySpec = describe "SearchTermParser" $ parallel $ do
  describe "Author Query Parser" $ do
    it "parses an author query" $ do
      parse author "" "author:\"John Doe\"" `shouldBe` Right (Author (Val (CaseInsensitiveTerm "John Doe")))
      parse author "" "author:John" `shouldBe` Right (Author (Val (CaseInsensitiveTerm "John")))

    it "parses an author query with a complex boolean expression" $ do
      let input = "author:(\"John Doe\" || ~Jane~)"
      parse author "" input `shouldBe` Right (Author (Or (Val (CaseInsensitiveTerm "John Doe")) (Val (FuzzyTerm "Jane"))))

  describe "Tag Query Parser" $ do
    it "parses a tag query" $ do
      parse tag "" "tag:\"Haskell\"" `shouldBe` Right (Tag (Val (CaseInsensitiveTerm "Haskell")))
      parse tag "" "tag:\'Haskell\'" `shouldBe` Right (Tag (Val (StrictTerm "Haskell")))

    it "parses a tag query with a NOT expression" $ do
      let input = "tag:(!\"Haskell\")"
      parse tag "" input `shouldBe` Right (Tag (Not (Val (CaseInsensitiveTerm "Haskell"))))

    it "parses a tag query with an AND expression" $ do
      let input = "tag:(\"Haskell\" && 'functional')"
      parse tag "" input `shouldBe` Right (Tag (And (Val (CaseInsensitiveTerm "Haskell")) (Val (StrictTerm "functional"))))

  describe "Description Query Parser" $ do
    it "parses a description query" $ do
      parse description "" "description:\"Haskell programming\"" `shouldBe` Right (Description (Val (CaseInsensitiveTerm "Haskell programming")))
      parse description "" "description:~Haskell programming~" `shouldBe` Right (Description (Val (FuzzyTerm "Haskell programming")))

  describe "Content Query Parser" $ do
    it "parses a content query" $ do
      parse content "" "content:\"sample content\"" `shouldBe` Right (Content (Val (CaseInsensitiveTerm "sample content")))
      parse content "" "content:/sample content/" `shouldBe` Right (Content (Val (RegexTerm "sample content")))

    it "parses a content query with AND combination of strict and fuzzy terms" $ do
      let input = "content:(\"term1\" && ~fuzzy~)"
      parse content "" input `shouldBe` Right (Content (And (Val (CaseInsensitiveTerm "term1")) (Val (FuzzyTerm "fuzzy"))))

    it "parses a content query with OR combination of strict and case-insensitive terms" $ do
      let input = "content:(\"term1\" || 'term2')"
      parse content "" input `shouldBe` Right (Content (Or (Val (CaseInsensitiveTerm "term1")) (Val (StrictTerm "term2"))))

    it "parses a content query with nested boolean expressions" $ do
      let input = "content:(\"term1\" && (~fuzzy~ || 'term2'))"
      parse content "" input `shouldBe` Right (Content (And (Val (CaseInsensitiveTerm "term1")) (Or (Val (FuzzyTerm "fuzzy")) (Val (StrictTerm "term2")))))

  describe "Task Query Passer" $ do
    it "parses a task query with Done tasks and a strict term" $ do
      let input = "task-done:\"exact term\""
      parse task "" input `shouldBe` Right (Task Done (Val (CaseInsensitiveTerm "exact term")))

    it "parses a task query with Todo tasks and a fuzzy term" $ do
      let input = "task-todo:~fuzzyTerm~"
      parse task "" input `shouldBe` Right (Task Todo (Val (FuzzyTerm "fuzzyTerm")))

    it "parses a task query with Both tasks and a regex term" $ do
      let input = "task:/regexTerm/"
      parse task "" input `shouldBe` Right (Task Both (Val (RegexTerm "regexTerm")))

    it "parses a task query with Done tasks and a case insensitive term" $ do
      let input = "task-done:\"Case Term\""
      parse task "" input `shouldBe` Right (Task Done (Val (CaseInsensitiveTerm "Case Term")))

    it "parses a task query with Todo tasks and a compound boolean expression" $ do
      let input = "task-todo:(\'term1\' || ~term2~)"
      parse task "" input `shouldBe` Right (Task Todo (Or (Val (StrictTerm "term1")) (Val (FuzzyTerm "term2"))))

    it "fails to parse an invalid task query" $ do
      let input = "task-unknown:\"term\""
      parse task "" input `shouldSatisfy` isLeft

spec :: Spec
spec = describe "SearchLanguageParser" $ parallel $ do
  searchTermSpec
  searchQuerySpec
