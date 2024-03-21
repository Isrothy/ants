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
    hasLink,
    completeQuery,
  )
where

import Commonmark.Extensions (AlertType (..))
import Data.Char (isPunctuation)
import Data.Functor
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601
import Model.DocQuery.BoolExpr (BoolExpr (..))
import Model.DocQuery.Query hiding (query)
import Model.DocQuery.Term (Term (..))
import Text.Parsec
import Text.Parsec.Text (Parser)
import Prelude hiding (and, any, not, or, (&&), (||))

class HasParser a where
  parser :: Parser a

spaced :: Parser a -> Parser a
spaced = between spaces spaces

parens :: Parser a -> Parser a
parens p = between (char '(') (char ')') (spaced p)

boolExpr :: (HasParser a) => Parser (BoolExpr a)
boolExpr = orExpr

notOp :: Parser (BoolExpr a -> BoolExpr a)
notOp = spaced (char '!') $> Not

orOp :: Parser (BoolExpr a -> BoolExpr a -> BoolExpr a)
orOp = spaced (string "||" *> notFollowedBy (char '|')) $> Or

andOp :: Parser (BoolExpr a -> BoolExpr a -> BoolExpr a)
andOp = spaced (string "&&" *> notFollowedBy (char '&')) $> And

orExpr :: (HasParser a) => Parser (BoolExpr a)
orExpr = andExpr `chainl1` try orOp

andExpr :: (HasParser a) => Parser (BoolExpr a)
andExpr = notExpr `chainl1` try andOp

notExpr :: (HasParser a) => Parser (BoolExpr a)
notExpr = (Not <$> (notOp *> notExpr)) <|> simpleExpr

prefixed :: String -> Parser a -> Parser a
prefixed l p = string l *> p

simpleExpr :: (HasParser a) => Parser (BoolExpr a)
simpleExpr = parens boolExpr <|> (parser <&> Val)

escapedChar :: Parser Char
escapedChar = char '\\' *> punctuation

punctuation :: Parser Char
punctuation = satisfy isPunctuation

unquotedTerm :: Parser Term
unquotedTerm = do
  text <- many1 alphaNum
  return $ CaseInsensitiveTerm $ T.pack text

quotedTerm :: Char -> (T.Text -> Term) -> Parser Term
quotedTerm quote constructor = do
  text <- between (char quote) (char quote) (many1 (escapedChar <|> noneOf [quote]))
  return $ constructor $ T.pack text

doubleQuotedTerm :: Parser Term
doubleQuotedTerm = quotedTerm '"' CaseInsensitiveTerm

singleQuotedTerm :: Parser Term
singleQuotedTerm = quotedTerm '\'' StrictTerm

regexTerm :: Parser Term
regexTerm = quotedTerm '/' RegexTerm

fuzzyTerm :: Parser Term
fuzzyTerm = quotedTerm '~' FuzzyTerm

term :: Parser Term
term = doubleQuotedTerm <|> singleQuotedTerm <|> regexTerm <|> fuzzyTerm <|> unquotedTerm

instance HasParser Term where
  parser = term

author :: Parser Query
author = prefixed "author:" $ Author <$> simpleExpr

tag :: Parser Query
tag = prefixed "tag:" $ Tag <$> simpleExpr

description :: Parser Query
description = prefixed "description:" $ Description <$> simpleExpr

content :: Parser Query
content = prefixed "content:" $ Content <$> simpleExpr

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

dayParser :: Parser Day
dayParser = do
  input <- many (oneOf "0123456789:TZ+-WP")
  case iso8601ParseM input of
    Just d -> return d
    Nothing -> parserFail "Invalid date format"

date :: Parser Query
date = prefixed "date:" (range <|> singleDay)
  where
    dayStartTime d = UTCTime d (timeOfDayToTime midnight)
    dayEndTime d = UTCTime (addDays 1 d) (timeOfDayToTime midnight)
    range = do
      _ <- char '['
      start <- optionMaybe $ dayStartTime <$> dayParser
      _ <- char ','
      end <- optionMaybe $ dayEndTime <$> dayParser
      _ <- char ']'
      return $ DateTimeRange start end
    singleDay = do
      d <- dayParser
      let start = Just $ dayStartTime d
      let end = Just $ dayEndTime d
      return $ DateTimeRange start end

hasLink :: Parser Query
hasLink = prefixed "has-link:" $ do
  linkText <- quotedLink <|> many1 (noneOf " \n")
  return $ HasLink $ T.pack linkText

quotedLink :: Parser String
quotedLink = between (char '"') (char '"') (many1 (escapedChar <|> noneOf "\""))

query :: Parser Query
query =
  choice
    [ try author,
      try alert,
      try content,
      try description,
      try date,
      try hasLink,
      try task,
      try tag
    ]

instance HasParser Query where
  parser = query

completeQuery :: Parser (BoolExpr Query)
completeQuery = boolExpr <* eof
