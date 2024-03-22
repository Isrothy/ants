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
simpleExpr = parens boolExpr <|> (parser <&> Var)

escapedChar :: Parser Char
escapedChar = char '\\' *> punctuation

punctuation :: Parser Char
punctuation = satisfy isPunctuation

quotedTerm :: Char -> (T.Text -> Term) -> Parser Term
quotedTerm quote constructor =
  constructor . T.pack <$> between (char quote) (char quote) (many1 (escapedChar <|> noneOf [quote]))

doubleQuotedTerm :: Parser Term
doubleQuotedTerm = quotedTerm '"' CaseInsensitiveTerm

singleQuotedTerm :: Parser Term
singleQuotedTerm = quotedTerm '\'' StrictTerm

regexTerm :: Parser Term
regexTerm = quotedTerm '/' RegexTerm

fuzzyTerm :: Parser Term
fuzzyTerm = quotedTerm '~' FuzzyTerm

unquotedTerm :: Parser Term
unquotedTerm = do
  text <- many1 alphaNum
  return $ CaseInsensitiveTerm $ T.pack text

term :: Parser Term
term = doubleQuotedTerm <|> singleQuotedTerm <|> regexTerm <|> fuzzyTerm <|> unquotedTerm

instance HasParser Term where
  parser = term

author :: Parser Query
author = Author <$> prefixed "author:" simpleExpr

tag :: Parser Query
tag = Tag <$> prefixed "tag:" simpleExpr

description :: Parser Query
description = Description <$> prefixed "description:" simpleExpr

content :: Parser Query
content = Content <$> prefixed "content:" simpleExpr

task :: Parser Query
task = Task <$> (string "task" *> taskType) <*> simpleExpr
  where
    taskType =
      char ':' $> Both
        <|> char '-' *> (string "done:" $> Done <|> string "todo:" $> Todo)

alert :: Parser Query
alert = Alert <$> (string "alert-" *> alertType) <*> simpleExpr
  where
    alertType =
      choice
        [ NoteAlert <$ string "note:",
          TipAlert <$ string "tip:",
          ImportantAlert <$ string "important:",
          WarningAlert <$ string "warning:",
          CautionAlert <$ string "caution:"
        ]

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
hasLink = HasLink . T.pack <$> prefixed "has-link:" (quotedString <|> many1 (noneOf " \n"))

quotedString :: Parser String
quotedString = between (char '"') (char '"') (many1 (escapedChar <|> noneOf "\""))

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
