module Parser.SearchLang
  ( booleanTerm,
    quotedTerm,
    unquotedTerm,
    regexTerm,
    fuzzyTerm,
    term,
  )
where

import Data.Algebra.Boolean
import Data.Char (isPunctuation, isSpace)
import Data.Functor
import qualified Data.Text as T
import qualified Model.DocFilter as F
import Text.Parsec
import Text.Parsec.Text (Parser)
import Prelude hiding (and, any, not, or, (&&), (||))

whiteSpace :: Parser ()
whiteSpace = do
  _ <- many space
  return ()

escapedChar :: Parser Char
escapedChar = do
  _ <- char '\\'
  c <- punctuation
  return c

punctuation :: Parser Char
punctuation = satisfy isPunctuation

quotedTerm :: Parser F.TextFilter
quotedTerm = do
  _ <- char '"'
  text <- many1 (escapedChar <|> noneOf "\"")
  _ <- char '"'
  return $ F.strictTerm $ T.pack text

unquotedTerm :: Parser F.TextFilter
unquotedTerm = do
  text <- many1 alphaNum
  return $ F.strictTerm $ T.pack text

regexTerm :: Parser F.TextFilter
regexTerm = do
  _ <- char '/'
  text <- many1 (escapedChar <|> noneOf "/")
  _ <- char '/'
  return $ F.regexTerm text

fuzzyTerm :: Parser F.TextFilter
fuzzyTerm = do
  _ <- char '~'
  text <- many1 (escapedChar <|> noneOf "~")
  _ <- char '~'
  return $ F.fuzzyTerm $ T.pack text

term :: Parser F.TextFilter
term = unquotedTerm <|> quotedTerm <|> regexTerm <|> fuzzyTerm

booleanTerm :: Parser F.TextFilter
booleanTerm = orTerm

orOperator :: Parser ()
orOperator = do
  _ <- string "||"
  _ <- lookAhead space
  return ()

notOperator :: Parser ()
notOperator = do
  _ <- char '!'
  return ()

orTerm :: Parser F.TextFilter
orTerm = do
  t1 <- andTerm
  rest <- many (try (whiteSpace >> orOperator >> whiteSpace >> andTerm))
  return $ or (t1 : rest)

andTerm :: Parser F.TextFilter
andTerm = do
  t1 <- notTerm
  rest <- many (try (whiteSpace >> notTerm))
  return $ and (t1 : rest)

notTerm :: Parser F.TextFilter
notTerm = (try (notOperator >> simpleTerm) <&> not) <|> simpleTerm

simpleTerm :: Parser F.TextFilter
simpleTerm = parens booleanTerm <|> term

parens :: Parser a -> Parser a
parens p = do
  _ <- char '('
  res <- p
  _ <- char ')'
  return res
