module Parser.MarkdownSpec
  ( spec,
  )
where

import Data.Either
import qualified Data.Text as T
import Model.MarkdownAst
import Parser.Markdown
import Test.Hspec

spec :: Spec
spec = describe "commonmark" $ do
  it "parses valid markdown correctly" $ do
    let result = markdownAst "test1" (T.pack "# Hello, world!")
    result `shouldSatisfy` (not . isLeft)

  it "parses headers correctly" $ do
    let result = markdownAst "test2" (T.pack "# Header 1")
    case result of
      Right (Just (MarkdownAst (Header 1 _) _ [])) -> True `shouldBe` True
      _ -> False `shouldBe` True

-- case result of
--   Right (Leaf (Header 1 _)) -> True `shouldBe` True
--   _ -> False `shouldBe` True
