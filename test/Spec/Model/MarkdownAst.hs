{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec.Model.MarkdownAst (spec) where

import Commonmark
import Data.Maybe
import qualified Data.Text as T
import Model.MarkdownAst
import Parser.Markdown
import Test.Hspec
import Text.RawString.QQ

sampleMarkdownDoc :: String
sampleMarkdownDoc =
  [r|# Header 1

This is a paragraph with **strong** emphasis and [a link](http://example.com).

- Item 1
- Item 2
  - Subitem 2.1
  - Subitem 2.2

> A blockquote with *emphasis*.
|]

nodeAtSpec :: Spec
nodeAtSpec = describe "nodeAt" $ do
  it "finds the header node correctly" $ do
    case markdownAst "test" (T.pack sampleMarkdownDoc) of
      Left _ -> error "Failed to parse markdown"
      Right ast -> do
        nodeAt isHeader 1 1 ast `shouldSatisfy` isJust
        nodeAt isHeader 1 9 ast `shouldSatisfy` isJust
        nodeAt isHeader 2 1 ast `shouldSatisfy` isNothing
        nodeAt isHeader 2 5 ast `shouldSatisfy` isNothing
        nodeAt isLink 3 49 ast `shouldSatisfy` isNothing

  it "finds the link correctly" $ do
    case markdownAst "test" (T.pack sampleMarkdownDoc) of
      Left _ -> error "Failed to parse markdown"
      Right ast -> do
        nodeAt isLink 2 52 ast `shouldSatisfy` isNothing
        nodeAt isLink 3 49 ast `shouldSatisfy` isNothing
        nodeAt isLink 3 50 ast `shouldSatisfy` isJust
        nodeAt isLink 3 52 ast `shouldSatisfy` isJust
        nodeAt isLink 3 57 ast `shouldSatisfy` isJust
        nodeAt isLink 3 58 ast `shouldSatisfy` isJust
        nodeAt isLink 3 59 ast `shouldSatisfy` isJust
        nodeAt isLink 3 77 ast `shouldSatisfy` isJust
        nodeAt isLink 3 78 ast `shouldSatisfy` isNothing
        nodeAt isLink 4 60 ast `shouldSatisfy` isNothing
  where
    isHeader (MarkdownAstNode (Header _ _) _ _) = True
    isHeader _ = False
    isLink (MarkdownAstNode (Link {}) _ _) = True
    isLink _ = False

spec :: Spec
spec = describe "Markdown AST Model" $ do
  nodeAtSpec
