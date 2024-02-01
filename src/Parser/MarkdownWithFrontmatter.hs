module Parser.MarkdownWithFrontmatter
  ( markdownWithFrontmatter,
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
markdownAstWith' ext file text = case markdownAstWith ext file text of
  Identity (Right ast) -> Just ast
  _ -> Nothing

markdownWithFrontmatter ::
  SyntaxSpec Identity MarkdownAst MarkdownAst ->
  String ->
  Parser (Maybe Metadata, Maybe MarkdownAst)
markdownWithFrontmatter ext file = do
  metadata <- optionMaybe frontmatter
  rest <- getInput
  return (metadata >>= decodeMaybeMetadata, markdownAstWith' ext file rest)
