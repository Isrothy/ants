module MarkdownParserSpec
  ( markdownAstSpec,
  )
where

import Data.Either
import qualified Data.Text as T
import Parser.MarkdownAst (MarkdownAst (..), MarkdownElement (..), markdownAst)
import Test.Hspec

markdownAstSpec :: Spec
markdownAstSpec = describe "commonmark" $ do
  it "parses valid markdown correctly" $ do
    let result = markdownAst "test1" (T.pack "# Hello, world!")
    result `shouldSatisfy` (not . isLeft)

  it "parses headers correctly" $ do
    let result = markdownAst "test2" (T.pack "# Header 1")
    case result of
      Right (Just (MarkdownAst (Header 1 _) _ [])) -> True `shouldBe` True
      -- Right (MarkdownAstNode (Span [MarkdownAstNode (Header 1 (MarkdownAstNode (Span [MarkdownAstNode (Text (T.pack "Header")) (Just _) [], MarkdownAstNode (Text (pack " ")) (Just _) [], MarkdownAstNode (Text (pack "1")) (Just _) []]) Nothing [])) (Just _) [], MarkdownAstNode (Paragraph Null) (Just _) []]) Nothing []) -> True `shouldBe` True
      _ -> False `shouldBe` True

-- case result of
--   Right (Leaf (Header 1 _)) -> True `shouldBe` True
--   _ -> False `shouldBe` True
