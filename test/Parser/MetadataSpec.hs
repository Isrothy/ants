{-# LANGUAGE OverloadedStrings #-}

module Parser.MetadataSpec
  ( spec,
  )
where

import Data.Either
import qualified Data.Text as T
import Parser.Metadata
import Test.Hspec
import Text.Parsec

spec :: Spec
spec = describe "metadataParser" $ parallel $ do
  it "parses metadata correctly when well-formed" $ do
    let input = "---\nkey: value\ranother: value2\r\n---\nrest of the file"
    let expectedResult = "key: value\ranother: value2\r\n"
    parse metadataParser "" (T.pack input) `shouldBe` Right (T.pack expectedResult)

  it "handles empty metadata" $ do
    let input = "---\n---\nrest of the file"
    parse metadataParser "" (T.pack input) `shouldBe` Right ""

  it "fails when no closing dash-line is found" $ do
    let input = "---\nkey: value\n"
    parse metadataParser "" (T.pack input) `shouldSatisfy` isLeft

  it "ignores content before the opening dash-line" $ do
    let input = "content before\n---\nkey: value\n---"
    parse metadataParser "" (T.pack input) `shouldSatisfy` isLeft

  it "handles metadata with multiple lines" $ do
    let input = "---\nline1\nline2\nline3\n---\n"
    let expectedResult = "line1\nline2\nline3\n"
    parse metadataParser "" (T.pack input) `shouldBe` Right (T.pack expectedResult)

  it "stops parsing metadata at the second dash-line" $ do
    let input = "---\nmetadata line\n---\nnot metadata\n---"
    let expectedResult = "metadata line\n"
    parse metadataParser "" (T.pack input) `shouldBe` Right (T.pack expectedResult)
