{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.SearchLangSpec
  ( spec,
  )
where

import Commonmark
import Data.Either
import qualified Data.Text as T
import Data.Time.Format.ISO8601
import qualified Model.DocFilter as F
import Model.Metadata
import Parser.SearchLang
import Test.Hspec
import Text.Parsec
import Text.RawString.QQ

searchTermSpec:: Spec
searchTermSpec = describe "SearchTermParser" $ parallel $ do
  describe "Basic term parsing" $ do
    it "parses a quoted term" $ do
      case parse quotedTerm "" "\"example\"" of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "example" `shouldBe` True
          F.filt t "example" `shouldBe` True
          F.filt t "  example  " `shouldBe` True
          F.filt t "ExAmple" `shouldBe` False
          F.filt t "xample" `shouldBe` False
    it "parses an unquoted term" $ do
      case parse unquotedTerm "" "example" of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "example" `shouldBe` True
          F.filt t "  example  " `shouldBe` True
          F.filt t "ExAmple" `shouldBe` False
          F.filt t "xample" `shouldBe` False

  describe "Regex term parsing" $ do
    it "parses a regex term" $ do
      case parse regexTerm "" "/[a-z]+/" of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "example" `shouldBe` True
          F.filt t "123" `shouldBe` False

  describe "Fuzzy term parsing" $ do
    it "parses a fuzzy term" $ do
      case parse fuzzyTerm "" "~example~" of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "example" `shouldBe` True
          F.filt t "exaample" `shouldBe` True
          F.filt t "ExAmple" `shouldBe` True
          F.filt t "xmpl" `shouldBe` False

  describe "Boolean operations parsing - OR" $ do
    it "parses an OR operation" $ do
      case parse booleanTerm "" "term1 || term2" of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "term1" `shouldBe` True
          F.filt t "term2" `shouldBe` True
          F.filt t "term3" `shouldBe` False
    it "parses an AND operation" $ do
      case parse booleanTerm "" "/term1/ term2" of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "term1 term2" `shouldBe` True
          F.filt t "term2 term1" `shouldBe` True
          F.filt t "term1" `shouldBe` False
          F.filt t "term2" `shouldBe` False

    it "parses a NOT operation" $ do
      case parse booleanTerm "" "!term1" of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "term1" `shouldBe` False
          F.filt t "other" `shouldBe` True

  describe "Complex expression parsing" $ do
    it "parses complex expressions" $ do
      case parse booleanTerm  "" "term1 (term2 || !term3)" of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "term1 term2" `shouldBe` True
          F.filt t "term1 term3" `shouldBe` False
          F.filt t "term1 other" `shouldBe` True
      case parse booleanTerm "" "~fuzzy~ /[a-z]+/ \"exact\"" of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "fuzzy example exact" `shouldBe` True
          F.filt t "yzuuf example exact" `shouldBe` False
          F.filt t "fuzzy exac" `shouldBe` False
    it "parses combinations of different operations and terms" $ do
      case parse booleanTerm "" "(term1 || !term2)  ~fuzzyTerm~ /regexTerm/" of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "term1 fuzzyTerm regexTerm" `shouldBe` True
          F.filt t "term2 fuzzyTerm regexTerm" `shouldBe` False
          F.filt t "term1 fuzzyTerm wrongRegex" `shouldBe` False
      case parse booleanTerm "" (T.pack "term1 || term2 || term3") of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "term1" `shouldBe` True
          F.filt t "term2" `shouldBe` True
          F.filt t "term3" `shouldBe` True
          F.filt t "term4" `shouldBe` False
      case parse booleanTerm "" (T.pack "!term1 !term2") of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "term1" `shouldBe` False
          F.filt t "term2" `shouldBe` False
          F.filt t "otherTerm" `shouldBe` True
      case parse booleanTerm "" (T.pack "term1 || !term2 || (term3 !term4)") of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "term1" `shouldBe` True
          F.filt t "term3" `shouldBe` True
          F.filt t "term2" `shouldBe` False
          F.filt t "term2 term3 term4" `shouldBe` False
          F.filt t "term2 term4 term5" `shouldBe` False
      case parse booleanTerm "" (T.pack "!(term1 || !term2)") of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "term1" `shouldBe` False
          F.filt t "term2" `shouldBe` True
          F.filt t "otherTerm" `shouldBe` False

  describe "Invalid NOT operations" $ do
    it "fails to parse double NOT operations (e.g., !!)" $ do
      case parse booleanTerm "" (T.pack "!!term1") of
        Right _ -> expectationFailure "Parser should not succeed on empty input"
        Left _ -> return ()
    it "fails to parse triple NOT operations (e.g., !!!)" $ do
      case parse booleanTerm "" (T.pack "!!!term1") of
        Right _ -> expectationFailure "Parser should not succeed on empty input"
        Left _ -> return ()
    it "fails to parse multiple consecutive NOT operations (e.g., !!)" $ do
      case parse booleanTerm "" (T.pack "!!!!term1") of
        Right _ -> expectationFailure "Parser should not succeed on empty input"
        Left _ -> return ()

  describe "Edge case handling" $ do
    it "handles an empty input" $ do
      case parse booleanTerm "" "" of
        Left _ -> return () -- Expecting a parse failure
        Right _ -> expectationFailure "Parser should not succeed on empty input"

spec :: Spec
spec = describe "SearchLanguageParser" $ parallel $ do
  searchTermSpec
