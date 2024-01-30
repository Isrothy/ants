module Parser.MarkdownWithFrontmatter
  ( markdownWithFrontmatter,
  )
where

import Commonmark
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Y
import Model.MarkdownAst
import Model.Metadata
import Parser.Markdown
import Parser.Frontmatter
import Text.Parsec
import Text.Parsec.Text

decodeMaybeMetadata :: T.Text -> Maybe Metadata
decodeMaybeMetadata input = case Y.decodeEither' (TE.encodeUtf8 input) of
  Left _ -> Nothing
  Right metadata -> Just metadata

markdownAstWith' ::
  SyntaxSpec Maybe (Maybe MarkdownAst) (Maybe MarkdownAst) ->
  String ->
  T.Text ->
  Maybe MarkdownAst
markdownAstWith' ext file text = case markdownAstWith ext file text of
  Just (Right ast) -> ast
  _ -> Nothing

markdownWithFrontmatter ::
  SyntaxSpec Maybe (Maybe MarkdownAst) (Maybe MarkdownAst) ->
  String ->
  Parser (Maybe Metadata, Maybe MarkdownAst)
markdownWithFrontmatter ext file = do
  metadata <- optionMaybe frontmatter
  rest <- getInput
  return (metadata >>= decodeMaybeMetadata, markdownAstWith' ext file rest)
