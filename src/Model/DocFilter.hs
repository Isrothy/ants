{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

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
    regularText,
    matchRelPath,
    matchRelPaths,
    hasLink,
  )
where

import Commonmark
import Data.Maybe
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
metadataFilter f = Filter $ \(Document _ m _ _) -> f m

astFilter :: (Maybe MarkdownAst -> Bool) -> DocFilter
astFilter f = Filter $ \(Document _ _ ast _) -> f ast

relPathFilter :: (Path Rel File -> Bool) -> DocFilter
relPathFilter f = Filter $ \(Document p _ _ _) -> f p

textFilter :: (T.Text -> Bool) -> DocFilter
textFilter f = Filter $ \(Document _ _ _ t) -> f t

dateRange :: Maybe UTCTime -> Maybe UTCTime -> DocFilter
dateRange start end = metadataFilter $ maybe False (between start end) . M.dateTime
  where
    between (Just s) (Just e) d = s <= e && d >= s && d <= e
    between (Just s) Nothing d = d >= s
    between Nothing (Just e) d = d <= e
    between _ _ _ = True

author :: T.Text -> DocFilter
author a = metadataFilter $ (Just a ==) . M.author

title :: T.Text -> DocFilter
title t = metadataFilter $ (Just t ==) . M.title

hasTag :: T.Text -> DocFilter
hasTag t = metadataFilter $ elem t . M.tags

fuzzyDescription :: T.Text -> DocFilter
fuzzyDescription t = metadataFilter $ Fuzzy.test t . M.description

keyword :: T.Text -> DocFilter
keyword t = astFilter $ Fuzzy.test t . toPlainText

keywords :: [T.Text] -> DocFilter
keywords ts = astFilter $ \ast -> any (\t -> Fuzzy.test t (toPlainText ast)) ts

strictKeyword :: T.Text -> DocFilter
strictKeyword t = textFilter $ T.isInfixOf t

strictKeywords :: [T.Text] -> DocFilter
strictKeywords ts = textFilter $ \t -> any (`T.isInfixOf` t) ts

regularText :: T.Text -> Bool
regularText = undefined

matchRelPath :: Path Rel File -> DocFilter
matchRelPath p = relPathFilter (== p)

matchRelPaths :: [Path Rel File] -> DocFilter
matchRelPaths ps = relPathFilter (`elem` ps)

hasLink :: Path Rel File -> DocFilter
hasLink p = astFilter $ \x -> case x of
  Nothing -> False
  Just ast -> Just p `elem` [parseRelFile $ T.unpack target | (target, _) <- findLinks ast]
