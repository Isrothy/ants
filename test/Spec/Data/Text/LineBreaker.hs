{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Data.Text.LineBreaker
  ( spec,
  )
where

import qualified Data.Text as T
import Data.Text.LineBreaker
import Test.Hspec

splitLinesSpec :: Spec
splitLinesSpec = describe "splitLines" $ do
  it "split lines correctly" $ do
    splitLines "Hello, world!" `shouldBe` [("Hello, world!", Nothing)]
    splitLines "Hello, world!\nHow are you today?\nI'm fine, thank you!"
      `shouldBe` [("Hello, world!", Just LF), ("How are you today?", Just LF), ("I'm fine, thank you!", Nothing)]
    splitLines "Hello, world!\r\nHow are you today?\rI'm fine, thank you!"
      `shouldBe` [("Hello, world!", Just CRLF), ("How are you today?", Just CR), ("I'm fine, thank you!", Nothing)]

joinLinesSpec :: Spec
joinLinesSpec = describe "joinLines" $ do
  it "joins lines correctly without line breakers" $ do
    joinLines [("Hello, world!", Nothing)] `shouldBe` "Hello, world!"

  it "joins lines correctly with LF line breakers" $ do
    joinLines [("Hello, world!", Just LF), ("How are you today?", Just LF), ("I'm fine, thank you!", Nothing)]
      `shouldBe` "Hello, world!\nHow are you today?\nI'm fine, thank you!"

  it "joins lines correctly with mixed line breakers" $ do
    joinLines [("Hello, world!", Just CRLF), ("How are you today?", Just CR), ("I'm fine, thank you!", Nothing)]
      `shouldBe` "Hello, world!\r\nHow are you today?\rI'm fine, thank you!"

spec :: Spec
spec = describe "Linke Breaker" $ do
  splitLinesSpec
  joinLinesSpec
