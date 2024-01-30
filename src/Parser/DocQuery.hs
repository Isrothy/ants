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
    alert,
    tag,
    date,
    description,
    content,
    task,
    completeQuery,
  )
where

import Commonmark.Extensions (AlertType (..))
import Data.Char (isPunctuation)
import Data.Functor
import qualified Data.Text as T
import Data.Time
import Data.Time.Format
import Data.Time.Format.ISO8601
import Model.DocQuery.BoolExpr (BoolExpr (..))
import Model.DocQuery.Query hiding (query)
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

simpleExpr :: (HasParser a) => Parser (BoolExpr a)
simpleExpr = parens boolExpr <|> (parser <&> Val)

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

task :: Parser Query
task = do
  _ <- string "task"
  t <- taskType
  Task t <$> simpleExpr
  where
    taskType =
      (char ':' >> return Both) <|> do
        _ <- char '-'
        (string "done:" >> return Done) <|> (string "todo:" >> return Todo)

alert :: Parser Query
alert = do
  _ <- string "alert-"
  t <- alertType
  Alert t <$> simpleExpr
  where
    alertType =
      (string "note:" >> return NoteAlert)
        <|> (string "tip:" >> return TipAlert)
        <|> (string "important:" >> return ImportantAlert)
        <|> (string "warning:" >> return WarningAlert)
        <|> (string "caution:" >> return CautionAlert)

day :: Parser Day
day = do
  input <- many (oneOf "0123456789:TZ+-WP")
  iso8601ParseM input

date :: Parser Query
date = do
  _ <- string "date:"
  range <|> singleDay
  where
    dayStartTime d = UTCTime d (timeOfDayToTime midnight)
    dayEndTime d = UTCTime (addDays 1 d) (timeOfDayToTime midnight)
    range = do
      _ <- char '['
      start <- optionMaybe $ dayStartTime <$> day
      _ <- char ','
      end <- optionMaybe $ dayEndTime <$> day
      _ <- char ']'
      return $ DateTimeRange start end
    singleDay = do
      d <- day
      let start = Just $ dayStartTime d
      let end = Just $ dayEndTime d
      return $ DateTimeRange start end

query :: Parser Query
query =
  try author
    <|> try alert
    <|> try content
    <|> try description
    <|> try date
    <|> try task
    <|> try tag

instance HasParser Query where
  parser = query

completeQuery :: Parser (BoolExpr Query)
completeQuery = do
  q <- boolExpr
  _ <- eof
  return q
