module Spec.Parser.Placeholder
  ( spec,
  )
where

import Commonmark
import Data.Functor.Identity
import qualified Data.Text as T
import Model.MarkdownAst
import Parser.Markdown
import Parser.Placeholder
import Test.Hspec

markdownAstWithPlaceholder ::
  String ->
  T.Text ->
  Identity (Either ParseError MarkdownAst)
markdownAstWithPlaceholder = markdownAstWith (placeholderSpec <> allSpecExtensions <> defaultSyntaxSpec)

spec :: Spec
spec = describe "placeholder" $ parallel $ do
  it "parses placeholder correctly" $ do
    let input = T.pack "## This is a {{placeholder}}."
    case runIdentity $ markdownAstWithPlaceholder "a" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        show (findPlaceholders ast)
          `shouldBe` "[AstNode {_parameters = PlaceholderParams {_text = \"placeholder\"}, _sourceRange = Just a@1:14-1:29, _attributes = []}]"

  it "parses text without placeholder correctly" $ do
    let input = T.pack "This is a regular text. \\{{ not a placeholder \\}}"
    case runIdentity $ markdownAstWithPlaceholder "filename" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        findPlaceholders ast `shouldBe` []

  it "parses placeholder with escaped char correctly" $ do
    let input = T.pack "## This is a {{placeholder \\} with escaped char }}"
    case runIdentity $ markdownAstWithPlaceholder "a" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        show (findPlaceholders ast)
          `shouldBe` "[AstNode {_parameters = PlaceholderParams {_text = \"placeholder \\\\} with escaped char \"}, _sourceRange = Just a@1:14-1:51, _attributes = []}]"

  it "parses multiple placeholders correctly" $ do
    let input = T.pack "## {{a placeholder}} \n **{{another placeholder}}** "
    case runIdentity $ markdownAstWithPlaceholder "a" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        show (findPlaceholders ast)
          `shouldBe` "[AstNode {_parameters = PlaceholderParams {_text = \"a placeholder\"}, _sourceRange = Just a@1:4-1:21, _attributes = []},AstNode {_parameters = PlaceholderParams {_text = \"another placeholder\"}, _sourceRange = Just a@2:4-2:27, _attributes = []}]"

  it "parses adjacent placeholders correctly" $ do
    let input = T.pack "- {{a placeholder}}{{another placeholder}}"
    case runIdentity $ markdownAstWithPlaceholder "a" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        show (findPlaceholders ast)
          `shouldBe` "[AstNode {_parameters = PlaceholderParams {_text = \"a placeholder\"}, _sourceRange = Just a@1:3-1:20, _attributes = []},AstNode {_parameters = PlaceholderParams {_text = \"another placeholder\"}, _sourceRange = Just a@1:20-1:43, _attributes = []}]"

  it "do not parse unclosed placeholder" $ do
    let input = T.pack "This is an {{unclosed placeholder."
    case runIdentity $ markdownAstWithPlaceholder "filename" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        findPlaceholders ast `shouldBe` []

  it "do not placeholder on different lines" $ do
    let input = T.pack "This is an {{unclosed \n placeholder}}."
    case runIdentity $ markdownAstWithPlaceholder "filename" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        findPlaceholders ast `shouldBe` []
