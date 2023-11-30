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
    matchRelPath,
    matchRelPaths,
  )
where

import Commonmark
import qualified Data.Text as T
import Data.Time
import Model.Document hiding (ast)
import Model.MarkdownAst
import qualified Model.Metadata as M
import Path
import qualified Text.Fuzzy as Fuzzy
import Util.Filter
import Prelude hiding (and, not, or)

type DocFilter = Filter Document

metadataFilter :: (M.Metadata -> Bool) -> DocFilter
metadataFilter f = Filter $ \(Document _ m _) -> f m

astFilter :: (Maybe MarkdownAst -> Bool) -> DocFilter
astFilter f = Filter $ \(Document _ _ ast) -> f ast

relPathFilter :: (Path Rel File -> Bool) -> DocFilter
relPathFilter f = Filter $ \(Document p _ _) -> f p

dateRange :: Maybe UTCTime -> Maybe UTCTime -> DocFilter
dateRange start end = metadataFilter $ \m -> case M.dateTime m of
  Nothing -> False
  Just d -> between start end
    where
      between (Just s) (Just e) = s <= e && d >= s && d <= e
      between (Just s) Nothing = d >= s
      between Nothing (Just e) = d <= e
      between Nothing Nothing = True

author :: T.Text -> DocFilter
author a = metadataFilter $ \m -> case M.author m of
  Nothing -> False
  Just a' -> a' == a

title :: T.Text -> DocFilter
title t = metadataFilter $ \m -> case M.title m of
  Nothing -> False
  Just t' -> t' == t

hasTag :: T.Text -> DocFilter
hasTag t = metadataFilter $ elem t . M.tags

fuzzyDescription :: T.Text -> DocFilter
fuzzyDescription t = metadataFilter $ \m -> Fuzzy.test t (M.description m)

keyword :: T.Text -> DocFilter
keyword t = astFilter $ Fuzzy.test t . toPlainText

keywords :: [T.Text] -> DocFilter
keywords ts = astFilter $ \ast -> any (\t -> Fuzzy.test t (toPlainText ast)) ts

strictKeyword :: T.Text -> DocFilter
strictKeyword t = astFilter $ T.isInfixOf t . toPlainText

strictKeywords :: [T.Text] -> DocFilter
strictKeywords ts = astFilter $ \ast -> any (\t -> T.isInfixOf t (toPlainText ast)) ts

matchRelPath :: Path Rel File -> DocFilter
matchRelPath p = relPathFilter (== p)

matchRelPaths :: [Path Rel File] -> DocFilter
matchRelPaths ps = relPathFilter $ \p -> p `elem` ps
