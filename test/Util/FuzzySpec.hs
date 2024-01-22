{-# LANGUAGE OverloadedStrings #-}

module Util.FuzzySpec
  ( spec,
  )
where

import Test.Hspec
import Util.Fuzzy

editDistanceSpec :: Spec
editDistanceSpec = describe "edit Distance" $ parallel $ do
  it "calculates the edit distance for identical lists" $ do
    editDistance "contains" "contains" `shouldBe` 0

  it "calculates the edit distance from empty to non-empty list" $ do
    editDistance "" "test" `shouldBe` 4

  it "calculates the edit distance from non-empty to empty list" $ do
    editDistance "test" "" `shouldBe` 4

  it "calculates the edit distance with one insertion" $ do
    editDistance "test" "tests" `shouldBe` 1

  it "calculates the edit distance with one deletion" $ do
    editDistance "tests" "test" `shouldBe` 1

  it "calculates the edit distance with one substitution" $ do
    editDistance "test" "best" `shouldBe` 1

  it "calculates the edit distance with multiple operations" $ do
    editDistance "kitten" "sitting" `shouldBe` 3

  it "calculates the edit distance for completely different lists" $ do
    editDistance "test" "abcd" `shouldBe` 4

  it "calculates the edit distance for longer strings with multiple operations" $ do
    editDistance "haskell" "pascal" `shouldBe` 4
    editDistance "unhappy" "united" `shouldBe` 5
    editDistance "jumping" "bumping" `shouldBe` 1
    editDistance "abcdefg" "azced" `shouldBe` 4
    editDistance "algorithm" "altruism" `shouldBe` 5
    editDistance "abc" "def" `shouldBe` 3
    editDistance "aaa" "aaaaa" `shouldBe` 2
    editDistance "complexity" "simplicity" `shouldBe` 4

minEditDistanceSubstringSpec :: Spec
minEditDistanceSubstringSpec = describe "minEditDistanceSubstring" $ parallel $ do
  it "calculates minimum edit distance for identical strings" $ do
    minEditDistanceSubstring "test" "test" `shouldBe` 0

  it "calculates minimum edit distance for completely different strings" $ do
    minEditDistanceSubstring "abc" "def" `shouldBe` 3
    minEditDistanceSubstring "abc" "bbc" `shouldBe` 1

  it "calculates minimum edit distance for one string contained within the other" $ do
    minEditDistanceSubstring "hello" "hello world" `shouldBe` 0

  it "calculates minimum edit distance with one insertion" $ do
    minEditDistanceSubstring "test" "test" `shouldBe` 0

  it "calculates minimum edit distance with one deletion" $ do
    minEditDistanceSubstring "tests" "test" `shouldBe` 1

  it "calculates minimum edit distance with one substitution" $ do
    minEditDistanceSubstring "test" "best" `shouldBe` 1

  it "calculates minimum edit distance with multiple operations" $ do
    minEditDistanceSubstring "kitten" "sitting" `shouldBe` 2

  it "calculates minimum edit distance for an empty string and a non-empty string" $ do
    minEditDistanceSubstring "" "test" `shouldBe` 0

  it "calculates minimum edit distance for non-empty string and an empty string" $ do
    minEditDistanceSubstring "test" "" `shouldBe` 4

  it "calculates minimum edit distance for strings with common prefix" $ do
    minEditDistanceSubstring "unhappy" "united" `shouldBe` 5

  it "calculates minimum edit distance for strings with common suffix" $ do
    minEditDistanceSubstring "jumping" "bumping" `shouldBe` 1

  it "calculates minimum edit distance for strings with interspersed common letters" $ do
    minEditDistanceSubstring "azced" "abcdefg" `shouldBe` 2

  it "calculates minimum edit distance for longer strings with partial matches" $ do
    minEditDistanceSubstring "subsequence" "submarineconsequence" `shouldBe` 3
    minEditDistanceSubstring "introspection" "retrospection" `shouldBe` 2
    minEditDistanceSubstring "ababab" "babababa" `shouldBe` 0
    minEditDistanceSubstring "abcdxyz" "xyzabcd" `shouldBe` 3
    minEditDistanceSubstring "abcdefg" "xabxcdxxefxgx" `shouldBe` 4

fuzzyContainsSpec :: Spec
fuzzyContainsSpec = describe "contains" $ parallel $ do
  it "returns True for empty and short strings" $ do
    contains "" "" `shouldBe` True
    contains "a" "a" `shouldBe` True
    contains "a" "ab" `shouldBe` True

  it "returns True for longer strings with exact matches" $ do
    contains "algorithm" "algorithmic" `shouldBe` True
    contains "haskell" "I love haskell programming" `shouldBe` True

  it "returns False for longer strings with high edit distances" $ do
    contains "haskell" "pascal" `shouldBe` False
    contains "functional" "imperative" `shouldBe` False

  it "handles variations in xs length" $ do
    contains "abcd" "abcdefg" `shouldBe` True
    contains "abcdefgh" "abcd" `shouldBe` False

  it "handles special cases" $ do
    contains "aaaa" "aaaabbbb" `shouldBe` True
    contains "abcd" "wxyz" `shouldBe` False

  it "handles longer strings with common substrings" $ do
    contains "longestcommonsubstring" "thisisthelongestcommonsubstringcontains" `shouldBe` True
    contains "longestcommonsubstring" "completelyunrelatedstring" `shouldBe` False

  it "handles longer strings with various edits" $ do
    contains "dynamicprogramming" "thisisdynamicprogtammingexample" `shouldBe` True
    contains "dynamicprogramming" "staticanalysis" `shouldBe` False
    contains "aaaaabbbbb" "aaaaabbbbbccccc" `shouldBe` True
    contains "aaaaabbbbb" "cccccaaaaaddddd" `shouldBe` False
    contains "insertdelete" "intertdelee" `shouldBe` True
    contains "insertdelete" "completelydifferent" `shouldBe` False
    contains "short" "averyveryverylongshostring" `shouldBe` True
    contains "averyveryverylongstring" "short" `shouldBe` False
    contains "partialmatch" "thisstringcontainsapartialmatchsomewhere" `shouldBe` True
    contains "partialmatch" "nosimilaritywhatsoever" `shouldBe` False

spec :: Spec
spec = do
  editDistanceSpec
  minEditDistanceSubstringSpec
  fuzzyContainsSpec
