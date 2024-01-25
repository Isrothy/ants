module Parser.DocSearch
  ( booleanTerm,
    simpleTerm,
    quotedTerm,
    unquotedTerm,
    regexTerm,
    fuzzyTerm,
    term,
    -- author,
    -- tag,
    -- description,
    -- content,
    -- rawTerm,
  )
where

import Data.Algebra.Boolean
import Data.Char (isPunctuation)
import Data.Functor
import qualified Data.Text as T
import qualified Model.DocFilter as F
import qualified Model.DocSearch
import Text.Parsec
import Text.Parsec.Text (Parser)
import Prelude hiding (and, any, not, or, (&&), (||))

parens :: Parser a -> Parser a
parens p = do
  _ <- char '('
  _ <- spaces
  res <- p
  _ <- spaces
  _ <- char ')'
  return res

spaces1 :: Parser ()
spaces1 = do
  _ <- many space
  return ()

escapedChar :: Parser Char
escapedChar = do
  _ <- char '\\'
  punctuation

punctuation :: Parser Char
punctuation = satisfy isPunctuation

quotedTerm :: Parser SearchTerm
quotedTerm = do
  _ <- char '"'
  text <- many1 (escapedChar <|> noneOf "\"")
  _ <- char '"'
  return $ StrictTerm $ T.pack text

unquotedTerm :: Parser SearchTerm
unquotedTerm = do
  text <- many1 alphaNum
  return $ StrictTerm $ T.pack text

regexTerm :: Parser SearchTerm
regexTerm = do
  _ <- char '/'
  text <- many1 (escapedChar <|> noneOf "/")
  _ <- char '/'
  return $ RegexTerm $ T.pack text

fuzzyTerm :: Parser SearchTerm
fuzzyTerm = do
  _ <- char '~'
  text <- many1 (escapedChar <|> noneOf "~")
  _ <- char '~'
  return $ FuzzyTerm $ T.pack text

term :: Parser SearchTerm
term = unquotedTerm <|> quotedTerm <|> regexTerm <|> fuzzyTerm

booleanTerm :: Parser SearchTerm
booleanTerm = orTerm

orOperator :: Parser ()
orOperator = do
  _ <- string "||"
  _ <- notFollowedBy $ char '|'
  return ()

notOperator :: Parser ()
notOperator = do
  _ <- char '!'
  return ()

orTerm :: Parser SearchTerm
orTerm = do
  t1 <- andTerm
  rest <- try (spaces >> orOperator >> spaces >> andTerm)
  return $ t1 Or rest

andTerm :: Parser SearchTerm
andTerm = do
  t1 <- notTerm
  rest <- try (spaces >> notTerm)
  return $ t1 And rest

notTerm ::  Parser SearchTerm
notTerm = (notOperator >> spaces >> simpleTerm <&> L.not) <|> simpleTerm

simpleTerm :: Parser SearchTerm
simpleTerm = parens booleanTerm <|> term

-- author :: Parser F.DocFilter
-- author = do
--   _ <- string "author:"
--   F.author <$> simpleTerm
--
-- tag :: Parser F.DocFilter
-- tag = do
--   _ <- string "tag:"
--   F.tag <$> simpleTerm
--
-- description :: Parser F.DocFilter
-- description = do
--   _ <- string "description:"
--   F.description <$> simpleTerm
--
-- content :: Parser F.DocFilter
-- content = do
--   _ <- string "content:"
--   F.content <$> simpleTerm
--
-- rawTerm :: Parser F.DocFilter
-- rawTerm = F.content <$> simpleTerm
