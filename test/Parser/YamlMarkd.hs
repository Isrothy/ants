{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parser.YamlMarkd
  ( spec,
  )
where

import Commonmark
import Data.Either (isRight)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Metadata
import Parser.MarkdownAst
import Parser.YamlMarked
import Test.Hspec
import Text.Parsec
import Text.RawString.QQ

spec :: Spec
spec = describe "markdownWithYamlParser" $ do
  it "parses a document with well-formed YAML header and Markdown content" $ do
    -- let input' = "---\ntitle: Test Document\nauthor: Joshua\ndate: 2023-01-01\ntags:\n  - Haskell\n  - Parsing\ndescription: A sample document\n---\n# Heading\nThis is a Markdown document with a YAML header."
    let input =
          [r|---
title: Test Document
author: Joshua
date: 2023-01-01
tags:
  - Haskell
  - Parsing
description: A sample document
---
# Heading
This is a Markdown document with a YAML header.
|]
    -- input' `shouldBe` input
    let expectedMetadata =
          Just $
            Metadata
              { title = Just "Test Document",
                author = Just "Joshua",
                date = Just "2023-01-01",
                tags = ["Haskell", "Parsing"],
                description = "A sample document"
              }
    let result = parse (markdownWithYamlParser defaultSyntaxSpec "test.md") "" (T.pack input)
    fst (fromRight (Nothing, Nothing) result) `shouldBe` expectedMetadata
    snd (fromRight (Nothing, Nothing) result) `shouldSatisfy` isJust

  it "handles documents without YAML header" $ do
    let input =
          [r|
# Heading
This is a Markdown document without a YAML header.
|]
    let result = parse (markdownWithYamlParser defaultSyntaxSpec "test.md") "" (T.pack input)
    fst (fromRight (Nothing, Nothing) result) `shouldBe` Nothing
    snd (fromRight (Nothing, Nothing) result) `shouldSatisfy` isJust

  it "handles malformed YAML headers" $ do
    let input =
          [r|---
This is not a valid YAML header
---
# Heading
Malformed YAML header above.
|]
    let result = parse (markdownWithYamlParser defaultSyntaxSpec "test.md") "" (T.pack input)
    fst (fromRight (Nothing, Nothing) result) `shouldBe` Nothing
    snd (fromRight (Nothing, Nothing) result) `shouldSatisfy` isJust

fromRight :: a -> Either b a -> a
fromRight _ (Right x) = x
fromRight def _ = def
