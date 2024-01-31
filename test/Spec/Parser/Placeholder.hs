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
    let input = T.pack "## This is a {placeholder}."
    case runIdentity $ markdownAstWithPlaceholder "a" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        show (findPlaceholders ast) `shouldBe` "[(\"placeholder\",a@1:14-1:27)]"

  it "parses text without placeholder correctly" $ do
    let input = T.pack "This is a regular text. \\{ not a placeholder \\}"
    case runIdentity $ markdownAstWithPlaceholder "filename" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        findPlaceholders ast `shouldBe` []

  it "parses placeholder with escaped char correctly" $ do
    let input = T.pack "## This is a {placeholder \\} with escaped char }"
    case runIdentity $ markdownAstWithPlaceholder "a" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        show (findPlaceholders ast) `shouldBe` "[(\"placeholder \\\\} with escaped char \",a@1:14-1:49)]"

  it "parses multiple placeholders correctly" $ do
    let input = T.pack "## {a placeholder} \n **{another placeholder}** "
    case runIdentity $ markdownAstWithPlaceholder "a" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        show (findPlaceholders ast) `shouldBe` "[(\"a placeholder\",a@1:4-1:19),(\"another placeholder\",a@2:4-2:25)]"

  it "parses adjacent placeholders correctly" $ do
    let input = T.pack "- {a placeholder}{another placeholder}"
    case runIdentity $ markdownAstWithPlaceholder "a" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        show (findPlaceholders ast) `shouldBe` "[(\"a placeholder\",a@1:3-1:18),(\"another placeholder\",a@1:18-1:39)]"

  it "do not parse unclosed placeholder" $ do
    let input = T.pack "This is an {unclosed placeholder."
    case runIdentity $ markdownAstWithPlaceholder "filename" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        findPlaceholders ast `shouldBe` []

  it "do not placeholder on different lines" $ do
    let input = T.pack "This is an {unclosed \n placeholder}."
    case runIdentity $ markdownAstWithPlaceholder "filename" input of
      Left parseError -> expectationFailure $ "Parsing failed: " ++ show parseError
      Right ast -> do
        findPlaceholders ast `shouldBe` []
