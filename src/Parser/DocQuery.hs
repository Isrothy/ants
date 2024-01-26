module Parser.DocQuery
  ( boolExpr,
    simpleExpr,
    singleQuotedTerm,
    doubleQuotedTerm,
    unquotedTerm,
    regexTerm,
    fuzzyTerm,
    term,
    author,
    tag,
    description,
    content,
    rawTerm,
  )
where

import Data.Char (isPunctuation)
import Data.Functor
import qualified Data.Text as T
import Model.DocQuery.BoolExpr (BoolExpr (..))
import Model.DocQuery.Query
import Model.DocQuery.Term (Term (..))
import Text.Parsec
import Text.Parsec.Text (Parser)
import Prelude hiding (and, any, not, or, (&&), (||))

class HasParser a where
  parser :: Parser a

parens :: Parser a -> Parser a
parens p = do
  _ <- char '('
  _ <- spaces
  res <- p
  _ <- spaces
  _ <- char ')'
  return res

boolExpr :: (HasParser a) => Parser (BoolExpr a)
boolExpr = orExpr

notOp :: Parser (BoolExpr a -> BoolExpr a)
notOp = do
  _ <- spaces
  _ <- char '!'
  _ <- spaces
  return Not

orOp :: Parser (BoolExpr a -> BoolExpr a -> BoolExpr a)
orOp = do
  _ <- spaces
  _ <- string "||"
  _ <- notFollowedBy $ char '|'
  _ <- spaces
  return Or

andOp :: Parser (BoolExpr a -> BoolExpr a -> BoolExpr a)
andOp = do
  _ <- spaces
  _ <- string "&&"
  _ <- notFollowedBy $ char '&'
  _ <- spaces
  return And

orExpr :: (HasParser a) => Parser (BoolExpr a)
orExpr = andExpr `chainl1` try orOp

andExpr :: (HasParser a) => Parser (BoolExpr a)
andExpr = notExpr `chainl1` try andOp

notExpr :: (HasParser a) => Parser (BoolExpr a)
notExpr =
  ( do
      op <- notOp
      op <$> notExpr
  )
    <|> simpleExpr

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

instance HasParser Term where
  parser = term

simpleExpr :: (HasParser a) => Parser (BoolExpr a)
simpleExpr = parens boolExpr <|> (parser <&> Val)

author :: Parser Query
author = do
  _ <- string "author:"
  Author <$> simpleExpr

tag :: Parser Query
tag = do
  _ <- string "tag:"
  Tag <$> simpleExpr

description :: Parser Query
description = do
  _ <- string "description:"
  Description <$> simpleExpr

content :: Parser Query
content = do
  _ <- string "content:"
  Content <$> simpleExpr

rawTerm :: Parser Query
rawTerm = Content <$> simpleExpr
