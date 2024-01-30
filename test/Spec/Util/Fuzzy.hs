{-# LANGUAGE OverloadedStrings #-}

module Spec.Util.Fuzzy
  ( spec,
  )
where

import Test.Hspec
import Util.Fuzzy

editDistanceSpec :: Spec
editDistanceSpec = describe "edit Distance" $ parallel $ do
  it "calculates the edit distance for identical lists" $ do
    editDistance "isInfixOf" "isInfixOf" `shouldBe` 0

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

fuzzyisInfixOfSpec :: Spec
fuzzyisInfixOfSpec = describe "isInfixOf" $ parallel $ do
  it "returns True for empty and short strings" $ do
    "" `isInfixOf` "" `shouldBe` True
    "a" `isInfixOf` "a" `shouldBe` True
    "a" `isInfixOf` "ab" `shouldBe` True

  it "returns True for longer strings with exact matches" $ do
    "algorithm" `isInfixOf` "algorithmic" `shouldBe` True
    "haskell" `isInfixOf` "I love haskell programming" `shouldBe` True

  it "returns False for longer strings with high edit distances" $ do
    "haskell" `isInfixOf` "pascal" `shouldBe` False
    "functional" `isInfixOf` "imperative" `shouldBe` False

  it "handles variations in xs length" $ do
    "abcd" `isInfixOf` "abcdefg" `shouldBe` True
    "abcdefgh" `isInfixOf` "abcd" `shouldBe` False

  it "handles special cases" $ do
    "aaaa" `isInfixOf` "aaaabbbb" `shouldBe` True
    "abcd" `isInfixOf` "wxyz" `shouldBe` False

  it "handles longer strings with common substrings" $ do
    "longestcommonsubstring" `isInfixOf` "thisisthelongestcommonsubstringisInfixOf" `shouldBe` True
    "longestcommonsubstring" `isInfixOf` "completelyunrelatedstring" `shouldBe` False

  it "handles longer strings with various edits" $ do
    "dynamicprogramming" `isInfixOf` "thisisdynamicprogtammingexample" `shouldBe` True
    "dynamicprogramming" `isInfixOf` "staticanalysis" `shouldBe` False
    "aaaaabbbbb" `isInfixOf` "aaaaabbbbbccccc" `shouldBe` True
    "aaaaabbbbb" `isInfixOf` "cccccaaaaaddddd" `shouldBe` False
    "insertdelete" `isInfixOf` "intertdelee" `shouldBe` True
    "insertdelete" `isInfixOf` "completelydifferent" `shouldBe` False
    "short" `isInfixOf` "averyveryverylongshostring" `shouldBe` True
    "averyveryverylongstring" `isInfixOf` "short" `shouldBe` False
    "partialmatch" `isInfixOf` "thisstringisInfixOfapartialmatchsomewhere" `shouldBe` True
    "partialmatch" `isInfixOf` "nosimilaritywhatsoever" `shouldBe` False

tIsInfixOfSpec :: Spec
tIsInfixOfSpec = describe "tIsInfixOf" $ parallel $ do
  it "returns True for identical texts" $ do
    "text" `isInfixOfT` "text" `shouldBe` True

  it "returns True for text and its uppercase version" $ do
    "text" `isInfixOfT` "TEXT" `shouldBe` True

  it "returns True for text with spaces and its trimmed version" $ do
    "  text  " `isInfixOfT` "text" `shouldBe` True

  it "returns True for a text contained within another with additional characters" $ do
    "haskell" `isInfixOfT` "I love haskell programming" `shouldBe` True

  it "returns False for unrelated texts" $ do
    "haskell" `isInfixOfT` "pascal" `shouldBe` False

  it "handles variations in text length" $ do
    "abcd" `isInfixOfT` "abcdefg" `shouldBe` True
    "abcdefgh" `isInfixOfT` "abcd" `shouldBe` False

  it "handles texts with common substrings" $ do
    "common" `isInfixOfT` "uncommonphrase" `shouldBe` True
    "common" `isInfixOfT` "completelydifferent" `shouldBe` False

  it "handles texts with special characters and spaces" $ do
    "special text" `isInfixOfT` "This is a special text with special characters!" `shouldBe` True
    "special*&text" `isInfixOfT` "specialcharactersintext" `shouldBe` False

  it "handles longer texts with various edits" $ do
    "dynamicprogramming" `isInfixOfT` "thisisdynamicprogtammingexample" `shouldBe` True
    "dynamicprogramming" `isInfixOfT` "staticanalysis" `shouldBe` False

spec :: Spec
spec = parallel $ do
  editDistanceSpec
  minEditDistanceSubstringSpec
  fuzzyisInfixOfSpec
  tIsInfixOfSpec
