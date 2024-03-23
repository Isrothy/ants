{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Data.Text.Encoding.Extra
  ( spec,
  )
where

import qualified Data.Text as T
import Data.Text.Encoding.Extra
import Test.Hspec

toUTF16OffsetSpec :: Spec
toUTF16OffsetSpec = describe "toUTF16Offset" $ do
  it "returns Nothing for offset beyond text length" $ do
    toUTF16Offset "hello" 6 `shouldBe` Nothing
  it "returns 0 for an empty string with offset 0" $ do
    toUTF16Offset "" 0 `shouldBe` Just 0
  it "correctly calculates offset for ASCII characters" $ do
    toUTF16Offset "hello" 5 `shouldBe` Just 10 -- "hello" is 5 characters, 2 bytes each in UTF-16LE
  it "correctly calculates offset for characters within BMP" $ do
    toUTF16Offset "你好" 2 `shouldBe` Just 4 -- Each Chinese character is 2 bytes in UTF-16LE
  it "correctly calculates offset including characters outside BMP" $ do
    toUTF16Offset "👍" 1 `shouldBe` Just 4 -- Emoji is outside BMP, 4 bytes in UTF-16LE
  it "handles mixed character sets" $ do
    toUTF16Offset "a👍" 2 `shouldBe` Just 6 -- 'a' (2 bytes) + emoji (4 bytes) in UTF-16LE
  it "calculates offset for a string with mixed character sets up to a non-BMP character" $ do
    toUTF16Offset "a👍bc" 2 `shouldBe` Just 6 -- Stops at the emoji, before 'bc'
  it "calculates offset for a complex string" $ do
    toUTF16Offset "Hello, 你好👍" 9 `shouldBe` Just 18 -- 'Hello, ' (14 bytes) + '你' (2 bytes) + '好' (2 bytes)

spec :: Spec
spec = describe "Text Encoding" $ do
  toUTF16OffsetSpec
