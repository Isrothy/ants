module Parser.MarkdownWithFrontmatter
  ( markdownWithFrontmatter,
    frontMatterLines,
  )
where

import Commonmark
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Y
import Model.MarkdownAst
import Model.Metadata
import Parser.Frontmatter
import Parser.Markdown
import Text.Parsec
import Text.Parsec.Text

decodeMaybeMetadata :: T.Text -> Maybe Metadata
decodeMaybeMetadata input = case Y.decodeEither' (TE.encodeUtf8 input) of
  Left _ -> Nothing
  Right metadata -> Just metadata

markdownAstWith' ::
  SyntaxSpec Identity MarkdownAst MarkdownAst ->
  String ->
  T.Text ->
  Maybe MarkdownAst
markdownAstWith' ext file text = case runIdentity $ markdownAstWith ext file text of
  (Right ast) -> Just ast
  _ -> Nothing

markdownWithFrontmatter ::
  SyntaxSpec Identity MarkdownAst MarkdownAst ->
  String ->
  T.Text ->
  (Maybe Metadata, Maybe MarkdownAst)
markdownWithFrontmatter ext file text =
  case parse frontmatter file text of
    Left _ -> (Nothing, markdownAstWith' ext file text)
    Right metadata ->
      let l = length $ T.lines metadata
          mdtext = replaceFirstLinesWithEmpty (l + 2) text
          ast = markdownAstWith' ext file mdtext
       in (decodeMaybeMetadata metadata, ast)

frontMatterLines :: T.Text -> Int
frontMatterLines text =
  case parse frontmatter "" text of
    Left _ -> 0
    Right metadata -> 2 + length (T.lines metadata)

replaceFirstLinesWithEmpty :: Int -> T.Text -> T.Text
replaceFirstLinesWithEmpty n text =
  let lines = T.lines text
      (toReplace, rest) = splitAt n lines
      replaced = replicate (length toReplace) T.empty
   in T.unlines $ replaced ++ rest
