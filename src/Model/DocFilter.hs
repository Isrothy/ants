{-# LANGUAGE GADTs #-}

module Model.DocFilter
  ( Match (..),
    DocFilter (..),
    and,
    or,
    not,
    xor,
    true,
    false,
    dateRange,
    author,
    title,
    hasTag,
    fuzzyDescription,
  )
where

import qualified Data.Text as T
import Data.Time
import Model.MarkdownAst
import qualified Model.Metadata as M
import qualified Text.Fuzzy as Fuzzy
import Prelude hiding (and, not, or)

data Match = Match
  { metadata :: Bool,
    lines :: [Int]
  }

data DocFilter where
  DocFilter :: {filt :: (M.Metadata, MarkdownAst) -> Maybe Match} -> DocFilter

and :: DocFilter -> DocFilter -> DocFilter
and (DocFilter f1) (DocFilter f2) = DocFilter $ \x -> case (f1 x, f2 x) of
  (Just (Match m1 l1), Just (Match m2 l2)) -> Just $ Match (m1 || m2) (l1 ++ l2)
  _ -> Nothing

or :: DocFilter -> DocFilter -> DocFilter
or (DocFilter f1) (DocFilter f2) = DocFilter $ \x -> case (f1 x, f2 x) of
  (Just (Match m1 l1), Just (Match m2 l2)) -> Just $ Match (m1 || m2) (l1 ++ l2)
  (Just (Match m1 l1), _) -> Just $ Match m1 l1
  (_, Just (Match m2 l2)) -> Just $ Match m2 l2
  _ -> Nothing

not :: DocFilter -> DocFilter
not (DocFilter f) = DocFilter $ \x -> case f x of
  Just (Match _ _) -> Nothing
  Nothing -> Just $ Match False []

xor :: DocFilter -> DocFilter -> DocFilter
xor (DocFilter f1) (DocFilter f2) = DocFilter $ \x -> case (f1 x, f2 x) of
  (Just m1, Nothing) -> Just m1
  (Nothing, Just m2) -> Just m2
  _ -> Nothing

true :: DocFilter
true = DocFilter $ const $ Just $ Match False []

false :: DocFilter
false = DocFilter $ const Nothing

instance Semigroup DocFilter where
  (<>) = and

instance Monoid DocFilter where
  mempty = true

fromBool :: Bool -> Maybe Match
fromBool True = Just $ Match True []
fromBool False = Nothing

dateRange :: Maybe UTCTime -> Maybe UTCTime -> DocFilter
dateRange start end = DocFilter $ \(m, _) -> case M.dateTime m of
  Nothing -> Nothing
  Just d -> fromBool (between start end)
    where
      between (Just s) (Just e) = s <= e && d >= s && d <= e
      between (Just s) Nothing = d >= s
      between Nothing (Just e) = d <= e
      between Nothing Nothing = True

author :: T.Text -> DocFilter
author a = DocFilter $ \(m, _) -> case M.author m of
  Nothing -> Nothing
  Just a' -> fromBool $ a' == a

title :: T.Text -> DocFilter
title t = DocFilter $ \(m, _) -> case M.title m of
  Nothing -> Nothing
  Just t' -> fromBool $ t' == t

hasTag :: T.Text -> DocFilter
hasTag t = DocFilter $ \(m, _) -> fromBool $ t `elem` M.tags m

fuzzyDescription :: T.Text -> DocFilter
fuzzyDescription t = DocFilter $ \(m, _) -> fromBool $ Fuzzy.test t (M.description m)
