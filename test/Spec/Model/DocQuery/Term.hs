{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Model.DocQuery.Term (spec) where

import qualified Data.Text as T
import Model.DocQuery.BoolExpr
import Model.DocQuery.Term
import Test.Hspec
import Text.RawString.QQ
import Prelude hiding (and, any, not, or)

fuzzyTermSpec :: Spec
fuzzyTermSpec = describe "fuzzyTerm Filter" $ parallel $ do
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

strictTermSpec :: Spec
strictTermSpec = describe "Strict Keywords Filter" $ parallel $ do
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

caseInsensitiveTermSpec :: Spec
caseInsensitiveTermSpec = describe "CaseInsensitiveTerm Tests" $ parallel $ do
  let doc = "Document with mixed CASE terms"

  it "matches terms regardless of case" $ do
    match (CaseInsensitiveTerm "mixed") doc `shouldBe` True

  it "matches exact terms irrespective of case differences" $ do
    match (CaseInsensitiveTerm "CASE") doc `shouldBe` True

regexTermSpec :: Spec
regexTermSpec = describe "Regex Matching Filter" $ parallel $ do
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

booleanTerms :: Spec
booleanTerms = describe "BooleanTerm Tests" $ parallel $ do
  describe "Or Operation Tests" $ do
    let doc = "Document with term Haskell but not Ruby"
    it "matches when at least one term is present" $ do
      match (Val (StrictTerm "Haskell") `Or` Val (StrictTerm "Ruby")) doc `shouldBe` True
    it "does not match when neither term is present" $ do
      match (Val (StrictTerm "Java") `Or` Val (StrictTerm "C++")) doc `shouldBe` False

  describe "And Operation Tests" $ do
    let doc = "Document with terms Haskell and Parsing"
    it "matches when both terms are present" $ do
      match (Val (StrictTerm "Haskell") `And` Val (StrictTerm "Parsing")) doc `shouldBe` True
    it "does not match when one term is absent" $ do
      match (Val (StrictTerm "Haskell") `And` Val (StrictTerm "Nonexistent")) doc `shouldBe` False

  describe "Not Operation Tests" $ do
    let doc = "Document without the term Ruby"
    it "matches when the term is absent" $ do
      match (Not $ Val $ StrictTerm "Ruby") doc `shouldBe` False
    it "does not match when the term is present" $ do
      match (Not $ Val $ StrictTerm "Haskell") doc `shouldBe` True

  describe "Nested And-Or Operations" $ do
    let doc = "Advanced document featuring Haskell, Scala, and functional programming concepts."

    it "matches complex nested And-Or conditions" $ do
      let term1 = And (Val (StrictTerm "Haskell")) (Val (StrictTerm "Scala"))
      let term2 = Or (Val (StrictTerm "functional")) (Val (StrictTerm "imperative"))
      match (And term1 term2) doc `shouldBe` True

    it "does not match if one of the nested conditions fails" $ do
      let term1 = And (Val (StrictTerm "Haskell")) (Val (StrictTerm "Ruby"))
      let term2 = Or (Val (StrictTerm "functional")) (Val (StrictTerm "imperative"))
      match (And term1 term2) doc `shouldBe` False

  describe "Complex Nested Not Operations" $ do
    let doc = "This document covers Haskell and functional programming but not OOP."

    it "matches with nested Not operations" $ do
      let term = Not (Val (StrictTerm "Java") `Or` Not (Val (StrictTerm "OOP")))
      match term doc `shouldBe` True

    it "does not match if the nested Not condition is true" $ do
      let term = Not (Val (StrictTerm "OOP") `Or` Not (Val (StrictTerm "Haskell")))
      match term doc `shouldBe` False

  describe "Boolean Combinations with Case Insensitivity" $ do
    let doc = "Document discussing PYTHON, java, and Ruby programming languages."

    it "matches mixed case terms with Or operation" $ do
      let term1 = Val $ CaseInsensitiveTerm "python"
      let term2 = Val $ CaseInsensitiveTerm "java"
      let term3 = Val $ CaseInsensitiveTerm "ruby"
      match (Or term1 (Or term2 term3)) doc `shouldBe` True

    it "does not match if none of the case-insensitive terms are present" $ do
      let term1 = Val $ CaseInsensitiveTerm "c++"
      let term2 = Val $ CaseInsensitiveTerm "haskell"
      match (Or term1 term2) doc `shouldBe` False

  describe "Mixed Fuzzy and Regex with Boolean Operations" $ do
    let doc = "Exploring various programming paradigms, including OOP, functional, and procedural."

    it "matches a combination of fuzzy and regex terms with And operation" $ do
      let term1 = Val $ FuzzyTerm "paradigm"
      let term2 = Val $ RegexTerm "OOP|functional|procedural"
      match (And term1 term2) doc `shouldBe` True

    it "does not match if the regex term fails in And operation" $ do
      let term1 = Val $ FuzzyTerm "paradigm"
      let term2 = Val $ RegexTerm "machine learning"
      match (And term1 term2) doc `shouldBe` False

spec :: Spec
spec = parallel $ do
  strictTermSpec
  caseInsensitiveTermSpec
  fuzzyTermSpec
  regexTermSpec
  booleanTerms
