{-# LANGUAGE LambdaCase #-}

module Parser.Main
  ( Parser (..),
    Position (..),
    ParserError (..),
    satisfy,
    char,
    anyChar,
    string,
    oneOf,
    digits,
    spaces,
    alwaysFail,
    optional,
    many,
    some,
    manyTill,
    lookAhead,
    choice,
    between,
    alphaNum,
    (<|>),
    (<*>),
  )
where

import Control.Applicative
import Data.Char

data Position = Position
  { line :: Int,
    column :: Int
  }
  deriving (Show, Eq)

data ParserError
  = UnexpectedChar Char Position
  | Mismatch Char Char Position
  | InputExhausted
  | CustomError String
  deriving (Show, Eq)

newtype Parser a = Parser
  { runParser :: Position -> String -> Either ParserError (a, Position, String)
  }

instance Functor Parser where
  fmap f p = Parser $ \pos input -> case runParser p pos input of
    Left err -> Left err
    Right (a, remaining, newPos) -> Right (f a, remaining, newPos)

instance Applicative Parser where
  pure a = Parser $ \pos input -> Right (a, pos, input)
  pf <*> pa = Parser $ \pos input -> case runParser pf pos input of
    Left err -> Left err
    Right (f, remaining1, newPos1) -> case runParser pa remaining1 newPos1 of
      Left err -> Left err
      Right (a, remaining2, newPos2) -> Right (f a, remaining2, newPos2)

instance Monad Parser where
  p >>= f = Parser $ \pos input -> case runParser p pos input of
    Left err -> Left err
    Right (a, remaining, newPos) -> runParser (f a) remaining newPos

instance Alternative Parser where
  empty = alwaysFail "Alternative: empty"
  p1 <|> p2 = Parser $ \pos input -> case runParser p1 pos input of
    Left _ -> runParser p2 pos input
    Right val -> Right val

updatePosition :: Char -> Position -> Position
updatePosition '\n' (Position l _) = Position (l + 1) 1
updatePosition _ (Position l c) = Position l (c + 1)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \pos -> \case
  [] -> Left InputExhausted
  (x : xs)
    | predicate x -> Right (x, updatePosition x pos, xs)
    | otherwise -> Left $ UnexpectedChar x pos

char :: Char -> Parser Char
char c = Parser $ \pos -> \case
  [] -> Left InputExhausted
  (x : xs)
    | x == c -> Right (x, updatePosition x pos, xs)
    | otherwise -> Left $ Mismatch c x pos

anyChar :: Parser Char
anyChar = satisfy (const True)

string :: String -> Parser String
string = traverse char

alwaysFail :: String -> Parser a
alwaysFail errorMessage = Parser $ \_ _ -> Left (CustomError errorMessage)

oneOf :: [Char] -> Parser Char
oneOf = foldr ((<|>) . char) (alwaysFail "oneOf: none of the characters match")

manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end = go
  where
    go = (end *> pure []) <|> (p >>= \x -> (x :) <$> go)

lookAhead :: Parser a -> Parser a
lookAhead p = Parser $ \pos input ->
  case runParser p pos input of
    Left err -> Left err
    Right (a, _, _) -> Right (a, pos, input)

choice :: [Parser a] -> Parser a
choice = foldr (<|>) (alwaysFail "None of the choices matched.")

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = do
  _ <- open
  result <- p
  _ <- close
  return result

digits :: Parser String
digits = some $ satisfy isDigit

spaces :: Parser String
spaces = many $ satisfy isSpace

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum <|> alwaysFail "Expected alphanumeric character"
