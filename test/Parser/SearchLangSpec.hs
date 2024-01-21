{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.SearchLangSpec
  ( spec,
  )
where

import Commonmark
import qualified Data.Text as T
import Data.Time.Format.ISO8601
import qualified Model.DocFilter as F
import Model.Metadata
import Parser.SearchLang
import Test.Hspec
import Text.Parsec
import Text.RawString.QQ

spec :: Spec
spec = describe "SearchLanguageParser" $ parallel $ do
  describe "Basic term parsing" $ do
    it "parses a quoted term" $ do
      case parse quotedTerm "" (T.pack "\"example\"") of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "example" `shouldBe` True
          F.filt t "example" `shouldBe` True
          F.filt t "  example  " `shouldBe` True
          F.filt t "ExAmple" `shouldBe` False
          F.filt t "xample" `shouldBe` False
    it "parses an unquoted term" $ do
      case parse unquotedTerm "" (T.pack "example") of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "example" `shouldBe` True
          F.filt t "  example  " `shouldBe` True
          F.filt t "ExAmple" `shouldBe` False
          F.filt t "xample" `shouldBe` False

  describe "Regex term parsing" $ do
    it "parses a regex term" $ do
      case parse regexTerm "" (T.pack "/[a-z]+/") of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "example" `shouldBe` True
          F.filt t "123" `shouldBe` False

  describe "Fuzzy term parsing" $ do
    it "parses a fuzzy term" $ do
      case parse fuzzyTerm "" (T.pack "~example~") of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "example" `shouldBe` True
          F.filt t "exaample" `shouldBe` True
          F.filt t "ExAmple" `shouldBe` True
          F.filt t "xmpl" `shouldBe` False

  describe "Boolean operations parsing - OR" $ do
    it "parses an OR operation" $ do
      case parse booleanTerm "" (T.pack "term1 || term2") of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "term1" `shouldBe` True
          F.filt t "term2" `shouldBe` True
          F.filt t "term3" `shouldBe` False
    it "parses an AND operation" $ do
      case parse booleanTerm "" (T.pack "/term1/ term2") of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "term1 term2" `shouldBe` True
          F.filt t "term2 term1" `shouldBe` True
          F.filt t "term1" `shouldBe` False
          F.filt t "term2" `shouldBe` False

    it "parses a NOT operation" $ do
      case parse booleanTerm "" (T.pack "!term1") of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "term" `shouldBe` False
          F.filt t "other" `shouldBe` True

  describe "Complex expression parsing" $ do
    it "parses complex expressions" $ do
      case parse booleanTerm "" (T.pack "term1 (term2 || !term3)") of
        Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
        Right t -> do
          F.filt t "term1 term2" `shouldBe` True
          F.filt t "term1 term3" `shouldBe` False
          F.filt t "term1 other" `shouldBe` True
