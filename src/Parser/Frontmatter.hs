module Parser.Frontmatter
  ( frontmatter,
  )
where

import Data.Maybe
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text (Parser)

eol :: Parser String
eol =
  string "\n" <|> do
    a <- char '\r'
    b <- optionMaybe (char '\n')
    return (a : maybeToList b)

dashLine :: Parser ()
dashLine = do
  _ <- string "---"
  _ <- manyTill space eol
  return ()

normalLine :: Parser String
normalLine = do
  text <- manyTill anyChar (lookAhead eol)
  eol' <- eol
  return (text ++ eol')

frontmatter :: Parser T.Text
frontmatter = do
  dashLine
  metadataContent <- manyTill normalLine (try dashLine)
  return (T.pack (concat metadataContent))
