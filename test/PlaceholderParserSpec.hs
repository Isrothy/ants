module PlaceholderParserSpec where

import Commonmark
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Parser.MarkdownAst
import Parser.Placeholder
import Test.Hspec

markdownAstWithPlaceholder ::
  String ->
  T.Text ->
  Either ParseError (Either ParseError (Maybe MarkdownAst))
markdownAstWithPlaceholder = markdownAstWith (placeholderSpec <> allSpecExtions <> defaultSyntaxSpec)

placeholderParserSpec :: Spec
placeholderParserSpec = describe "placeholder" $ do
  it "parses placeholder correctly" $ do
    let input = "This is a {placeholder}."
    let result = markdownAstWithPlaceholder "a" (T.pack input)
    result `shouldSatisfy` isRight

-- it "parses text without placeholder correctly" $ do
--   let input = "This is a regular text."
--   let result = markdownAstWithPlaceholder input
--   result `shouldSatisfy` isRight
--   let (Right (Just ast)) = result
--   ast `shouldBe` MarkdownAst [PlainText "This is a regular text."]

-- it "handles placeholder at the beginning of the text" $ do
--   let input = "{placeholder} at the beginning."
-- ... similarly as above

-- it "handles placeholder at the end of the text" $ do
--   let input = "Placeholder at the end {placeholder}."
-- ... similarly as above

-- it "handles adjacent placeholders" $ do
--   let input = "Adjacent {placeholder}{placeholder}."
-- ... similarly as above

-- it "handles escaped placeholders" $ do
-- let input = "This is not a \\{placeholder}."
-- ... similarly as above

-- it "returns an error for unclosed placeholders" $ do
--   let input = "This is an {unclosed placeholder."
--   let result = markdownAstWithPlaceholder input
--   result `shouldSatisfy` isLeft

expectedAst :: MarkdownAst
expectedAst =
  MarkdownAst
    [ PlainText "This is a ",
      Placeholder "placeholder",
      PlainText "."
    ]
