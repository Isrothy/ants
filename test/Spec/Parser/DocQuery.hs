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
      parse booleanTerm "" "term1 || term2" `shouldBe` Right (CaseInsensitiveTerm "term1" `Or` CaseInsensitiveTerm "term2")
      parse booleanTerm "" "term1||  term2" `shouldBe` Right (CaseInsensitiveTerm "term1" `Or` CaseInsensitiveTerm "term2")
      parse booleanTerm "" "term1 ||term2" `shouldBe` Right (CaseInsensitiveTerm "term1" `Or` CaseInsensitiveTerm "term2")
      parse booleanTerm "" "term1||term2" `shouldBe` Right (CaseInsensitiveTerm "term1" `Or` CaseInsensitiveTerm "term2")

    it "parses an AND operation" $ do
      parse booleanTerm "" "/term1/ && term2" `shouldBe` Right (RegexTerm "term1" `And` CaseInsensitiveTerm "term2")
      parse booleanTerm "" "/term1/&& term2" `shouldBe` Right (RegexTerm "term1" `And` CaseInsensitiveTerm "term2")
      parse booleanTerm "" "/term1/   &&term2" `shouldBe` Right (RegexTerm "term1" `And` CaseInsensitiveTerm "term2")
      parse booleanTerm "" "/term1/&&term2" `shouldBe` Right (RegexTerm "term1" `And` CaseInsensitiveTerm "term2")

    it "parses a NOT operation" $ do
      parse booleanTerm "" "!term1" `shouldBe` Right (Not (CaseInsensitiveTerm "term1"))
      parse booleanTerm "" "! term1" `shouldBe` Right (Not (CaseInsensitiveTerm "term1"))
      parse booleanTerm "" "!   term1" `shouldBe` Right (Not (CaseInsensitiveTerm "term1"))

  describe "Complex expression parsing" $ do
    it "parses complex expressions" $ do
      parse booleanTerm "" "term1 && (term2 || !term3)"
        `shouldBe` Right
          (CaseInsensitiveTerm "term1" `And` (CaseInsensitiveTerm "term2" `Or` Not (CaseInsensitiveTerm "term3")))
      parse booleanTerm "" "~fuzzy~ && /[a-z]+/ && \"exact\""
        `shouldBe` Right (FuzzyTerm "fuzzy" `And` RegexTerm "[a-z]+" `And` CaseInsensitiveTerm "exact")
    it "parses combinations of different operations and terms" $ do
      parse booleanTerm "" "(term1)" `shouldBe` Right (CaseInsensitiveTerm "term1")
      parse booleanTerm "" "((term1))" `shouldBe` Right (CaseInsensitiveTerm "term1")
      parse booleanTerm "" "((  (term1 )) )" `shouldBe` Right (CaseInsensitiveTerm "term1")
      parse booleanTerm "" "(term1 || !term2)  && ~fuzzyTerm~ && /regexTerm/"
        `shouldBe` Right
          ( (CaseInsensitiveTerm "term1" `Or` Not (CaseInsensitiveTerm "term2"))
              `And` FuzzyTerm "fuzzyTerm"
              `And` RegexTerm "regexTerm"
          )
      parse booleanTerm "" "term1 || term2 || (term3)"
        `shouldBe` Right
          (CaseInsensitiveTerm "term1" `Or` CaseInsensitiveTerm "term2" `Or` CaseInsensitiveTerm "term3")
      parse booleanTerm "" "!term1  && !term2"
        `shouldBe` Right
          (Not (CaseInsensitiveTerm "term1") `And` Not (CaseInsensitiveTerm "term2"))
      parse booleanTerm "" "term1 || ! term2 || ( term3 && !(term4))"
        `shouldBe` Right
          ( CaseInsensitiveTerm "term1"
              `Or` Not (CaseInsensitiveTerm "term2")
              `Or` (CaseInsensitiveTerm "term3" `And` Not (CaseInsensitiveTerm "term4"))
          )
      parse booleanTerm "" "!(  term1 || !term2)"
        `shouldBe` Right (Not (CaseInsensitiveTerm "term1" `Or` Not (CaseInsensitiveTerm "term2")))

  describe "Multiple NOT operations" $ do
    it "fails to parse double NOT operations (e.g., !!)" $ do
      parse booleanTerm "" "!!term1" `shouldBe` Right (Not (Not (CaseInsensitiveTerm "term1")))
      parse booleanTerm "" "! !term1" `shouldBe` Right (Not (Not (CaseInsensitiveTerm "term1")))
      parse booleanTerm "" "!! term1" `shouldBe` Right (Not (Not (CaseInsensitiveTerm "term1")))
      parse booleanTerm "" "! !  term1" `shouldBe` Right (Not (Not (CaseInsensitiveTerm "term1")))
    it "fails to parse triple NOT operations (e.g., !!!)" $ do
      parse booleanTerm "" "!!!term1" `shouldBe` Right (Not (Not (Not (CaseInsensitiveTerm "term1"))))
      parse booleanTerm "" "!!! term1" `shouldBe` Right (Not (Not (Not (CaseInsensitiveTerm "term1"))))
      parse booleanTerm "" "! !!term1" `shouldBe` Right (Not (Not (Not (CaseInsensitiveTerm "term1"))))
    it "fails to parse multiple consecutive NOT operations (e.g., !!)" $ do
      parse booleanTerm "" "!! term1 && ! !term2"
        `shouldBe` Right (Not (Not (CaseInsensitiveTerm "term1")) `And` Not (Not (CaseInsensitiveTerm "term2")))
      parse booleanTerm "" "!!term1 && ! ! term2 && ! !term3"
        `shouldBe` Right
          ( Not (Not (CaseInsensitiveTerm "term1"))
              `And` Not (Not (CaseInsensitiveTerm "term2"))
              `And` Not (Not (CaseInsensitiveTerm "term3"))
          )

  describe "Edge case handling" $ do
    it "handles an empty input" $ do
      parse booleanTerm "" "" `shouldSatisfy` isLeft

spec :: Spec
spec = describe "SearchLanguageParser" $ parallel $ do
  searchTermSpec
