module Parser.DocQuery
  ( booleanTerm,
    simpleTerm,
    singleQuotedTerm,
    doubleQuotedTerm,
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

import Data.Char (isPunctuation)
import Data.Functor
import qualified Data.Text as T
import Model.DocQuery.Query
import Model.DocQuery.Term
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

escapedChar :: Parser Char
escapedChar = do
  _ <- char '\\'
  punctuation

punctuation :: Parser Char
punctuation = satisfy isPunctuation

doubleQuotedTerm :: Parser Term
doubleQuotedTerm = do
  _ <- char '"'
  text <- many1 (escapedChar <|> noneOf "\"")
  _ <- char '"'
  return $ CaseInsensitiveTerm $ T.pack text

singleQuotedTerm :: Parser Term
singleQuotedTerm = do
  _ <- char '\''
  text <- many1 (escapedChar <|> noneOf "\'")
  _ <- char '\''
  return $ StrictTerm $ T.pack text

unquotedTerm :: Parser Term
unquotedTerm = do
  text <- many1 alphaNum
  return $ CaseInsensitiveTerm $ T.pack text

regexTerm :: Parser Term
regexTerm = do
  _ <- char '/'
  text <- many1 (escapedChar <|> noneOf "/")
  _ <- char '/'
  return $ RegexTerm $ T.pack text

fuzzyTerm :: Parser Term
fuzzyTerm = do
  _ <- char '~'
  text <- many1 (escapedChar <|> noneOf "~")
  _ <- char '~'
  return $ FuzzyTerm $ T.pack text

term :: Parser Term
term = doubleQuotedTerm <|> singleQuotedTerm <|> regexTerm <|> fuzzyTerm <|> unquotedTerm

booleanTerm :: Parser Term
booleanTerm = orTerm

notOperator :: Parser ()
notOperator = do
  _ <- char '!'
  return ()

orTerm :: Parser Term
orTerm = andTerm `chainl1` try orOp

andTerm :: Parser Term
andTerm = notTerm `chainl1` try andOp

orOp :: Parser (Term -> Term -> Term)
orOp = do
  _ <- spaces
  _ <- string "||"
  _ <- notFollowedBy $ char '|'
  _ <- spaces
  return Or

andOp :: Parser (Term -> Term -> Term)
andOp = do
  _ <- spaces
  _ <- string "&&"
  _ <- notFollowedBy $ char '&'
  _ <- spaces
  return And

notTerm :: Parser Term
notTerm = (notOperator >> spaces >> notTerm <&> Not) <|> simpleTerm

simpleTerm :: Parser Term
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
