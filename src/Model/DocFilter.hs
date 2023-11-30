{-# LANGUAGE GADTs #-}

module Model.DocFilter
  ( DocFilter,
    dateRange,
    author,
    title,
    hasTag,
    fuzzyDescription,
    keyword,
    keywords,
    strictKeyword,
    strictKeywords,
  )
where

import Commonmark
import qualified Data.Text as T
import Data.Time
import Model.MarkdownAst
import qualified Model.Metadata as M
import qualified Text.Fuzzy as Fuzzy
import Util.Filter
import Prelude hiding (and, not, or)

type DocFilter = Filter (M.Metadata, Maybe MarkdownAst)

dateRange :: Maybe UTCTime -> Maybe UTCTime -> DocFilter
dateRange start end = Filter $ \(m, _) -> case M.dateTime m of
  Nothing -> False
  Just d -> between start end
    where
      between (Just s) (Just e) = s <= e && d >= s && d <= e
      between (Just s) Nothing = d >= s
      between Nothing (Just e) = d <= e
      between Nothing Nothing = True

author :: T.Text -> DocFilter
author a = Filter $ \(m, _) -> case M.author m of
  Nothing -> False
  Just a' -> a' == a

title :: T.Text -> DocFilter
title t = Filter $ \(m, _) -> case M.title m of
  Nothing -> False
  Just t' -> t' == t

hasTag :: T.Text -> DocFilter
hasTag t = Filter $ \(m, _) -> t `elem` M.tags m

fuzzyDescription :: T.Text -> DocFilter
fuzzyDescription t = Filter $ \(m, _) -> Fuzzy.test t (M.description m)

keyword :: T.Text -> DocFilter
keyword t = Filter $ \(_, ast) -> Fuzzy.test t (toPlainText ast)

keywords :: [T.Text] -> DocFilter
keywords ts = Filter $ \(_, ast) -> any (\t -> Fuzzy.test t (toPlainText ast)) ts

strictKeyword :: T.Text -> DocFilter
strictKeyword t = Filter $ \(_, ast) -> T.isInfixOf t (toPlainText ast)

strictKeywords :: [T.Text] -> DocFilter
strictKeywords ts = Filter $ \(_, ast) -> any (\t -> T.isInfixOf t (toPlainText ast)) ts
