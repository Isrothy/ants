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
    it "parses a quotesearchTermd term" $ do
      let input = T.pack "\"example\""
      parse quotedTerm "" input `shouldBe` Right (StrictTerm "example")

    it "parses an unquoted term" $ do
      let input = T.pack "example"
      parse term "" input `shouldBe` Right (StrictTerm "example")

  describe "Regex term parsing" $ do
    let input = T.pack "/[a-z]+/"
    it "parses a regex term" $ do
      parse regexTerm "" input `shouldBe` Right (RegexTerm "[a-z]+")

  describe "Fuzzy term parsing" $ do
    let input = T.pack "~example~"
    it "parses a fuzzy term" $ do
      parse fuzzyTerm "" input `shouldBe` Right (FuzzyTerm "example")

  describe "Boolean operations parsing" $ do
    it "parses an OR operation" $ do
      parse booleanTerm "" "term1 || term2" `shouldBe` Right (StrictTerm "term1" `Or` StrictTerm "term2")
      parse booleanTerm "" "term1||  term2" `shouldBe` Right (StrictTerm "term1" `Or` StrictTerm "term2")
      parse booleanTerm "" "term1 ||term2" `shouldBe` Right (StrictTerm "term1" `Or` StrictTerm "term2")
      parse booleanTerm "" "term1||term2" `shouldBe` Right (StrictTerm "term1" `Or` StrictTerm "term2")

    it "parses an AND operation" $ do
      parse booleanTerm "" "/term1/ && term2" `shouldBe` Right (RegexTerm "term1" `And` StrictTerm "term2")
      parse booleanTerm "" "/term1/&& term2" `shouldBe` Right (RegexTerm "term1" `And` StrictTerm "term2")
      parse booleanTerm "" "/term1/   &&term2" `shouldBe` Right (RegexTerm "term1" `And` StrictTerm "term2")
      parse booleanTerm "" "/term1/&&term2" `shouldBe` Right (RegexTerm "term1" `And` StrictTerm "term2")

    it "parses a NOT operation" $ do
      parse booleanTerm "" "!term1" `shouldBe` Right (Not (StrictTerm "term1"))
      parse booleanTerm "" "! term1" `shouldBe` Right (Not (StrictTerm "term1"))
      parse booleanTerm "" "!   term1" `shouldBe` Right (Not (StrictTerm "term1"))

  describe "Complex expression parsing" $ do
    it "parses complex expressions" $ do
      parse booleanTerm "" "term1 && (term2 || !term3)"
        `shouldBe` Right
          (StrictTerm "term1" `And` (StrictTerm "term2" `Or` Not (StrictTerm "term3")))
      parse booleanTerm "" "~fuzzy~ && /[a-z]+/ && \"exact\""
        `shouldBe` Right (FuzzyTerm "fuzzy" `And` RegexTerm "[a-z]+" `And` StrictTerm "exact")
    it "parses combinations of different operations and terms" $ do
      parse booleanTerm "" "(term1)" `shouldBe` Right (StrictTerm "term1")
      parse booleanTerm "" "((term1))" `shouldBe` Right (StrictTerm "term1")
      parse booleanTerm "" "((  (term1 )) )" `shouldBe` Right (StrictTerm "term1")
      parse booleanTerm "" "(term1 || !term2)  && ~fuzzyTerm~ && /regexTerm/"
        `shouldBe` Right
          ( (StrictTerm "term1" `Or` Not (StrictTerm "term2"))
              `And` FuzzyTerm "fuzzyTerm"
              `And` RegexTerm "regexTerm"
          )
      parse booleanTerm "" "term1 || term2 || (term3)"
        `shouldBe` Right
          (StrictTerm "term1" `Or` StrictTerm "term2" `Or` StrictTerm "term3")
      parse booleanTerm "" "!term1  && !term2"
        `shouldBe` Right
          (Not (StrictTerm "term1") `And` Not (StrictTerm "term2"))
      parse booleanTerm "" "term1 || ! term2 || ( term3 && !(term4))"
        `shouldBe` Right
          ( StrictTerm "term1"
              `Or` Not (StrictTerm "term2")
              `Or` (StrictTerm "term3" `And` Not (StrictTerm "term4"))
          )
      parse booleanTerm "" "!(  term1 || !term2)"
        `shouldBe` Right (Not (StrictTerm "term1" `Or` Not (StrictTerm "term2")))

  describe "Multiple NOT operations" $ do
    it "fails to parse double NOT operations (e.g., !!)" $ do
      parse booleanTerm "" "!!term1" `shouldBe` Right (Not (Not (StrictTerm "term1")))
      parse booleanTerm "" "! !term1" `shouldBe` Right (Not (Not (StrictTerm "term1")))
      parse booleanTerm "" "!! term1" `shouldBe` Right (Not (Not (StrictTerm "term1")))
      parse booleanTerm "" "! !  term1" `shouldBe` Right (Not (Not (StrictTerm "term1")))
    it "fails to parse triple NOT operations (e.g., !!!)" $ do
      parse booleanTerm "" "!!!term1" `shouldBe` Right (Not (Not (Not (StrictTerm "term1"))))
      parse booleanTerm "" "!!! term1" `shouldBe` Right (Not (Not (Not (StrictTerm "term1"))))
      parse booleanTerm "" "! !!term1" `shouldBe` Right (Not (Not (Not (StrictTerm "term1"))))
    it "fails to parse multiple consecutive NOT operations (e.g., !!)" $ do
      parse booleanTerm "" "!! term1 && ! !term2"
        `shouldBe` Right (Not (Not (StrictTerm "term1")) `And` Not (Not (StrictTerm "term2")))
      parse booleanTerm "" "!!term1 && ! ! term2 && ! !term3"
        `shouldBe` Right
          ( Not (Not (StrictTerm "term1"))
              `And` Not (Not (StrictTerm "term2"))
              `And` Not (Not (StrictTerm "term3"))
          )

  describe "Edge case handling" $ do
    it "handles an empty input" $ do
      parse booleanTerm "" "" `shouldSatisfy` isLeft

spec :: Spec
spec = describe "SearchLanguageParser" $ parallel $ do
  searchTermSpec
