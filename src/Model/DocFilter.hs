{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Model.DocFilter
  ( DocFilter (..),
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
import Data.Algebra.Boolean
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Model.Document
import Model.MarkdownAst
import qualified Model.Metadata as M
import Path
import qualified Text.Fuzzy as Fuzzy
import Prelude hiding (and, any, not, or, (&&), (||))

newtype DocFilter where
  Filter :: {filt :: Document -> Bool} -> DocFilter

instance Boolean DocFilter where
  true = Filter $ const True
  false = Filter $ const False
  (&&) f1 f2 = Filter $ \x -> filt f1 x && filt f2 x
  (||) f1 f2 = Filter $ \x -> filt f1 x || filt f2 x
  not f = Filter $ \x -> not $ filt f x

metadataFilter :: (M.Metadata -> Bool) -> DocFilter
metadataFilter f = Filter $ f . metadata

astFilter :: (Maybe MarkdownAst -> Bool) -> DocFilter
astFilter f = Filter $ f . ast

relPathFilter :: (Path Rel File -> Bool) -> DocFilter
relPathFilter f = Filter $ f . relPath

textFilter :: (T.Text -> Bool) -> DocFilter
textFilter f = Filter $ f . text

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
keywords = any keyword

strictKeyword :: T.Text -> DocFilter
strictKeyword = textFilter . T.isInfixOf

strictKeywords :: [T.Text] -> DocFilter
strictKeywords = any strictKeyword

regularText :: T.Text -> DocFilter
regularText = undefined

matchRelPath :: Path Rel File -> DocFilter
matchRelPath = relPathFilter . (==)

matchRelPaths :: [Path Rel File] -> DocFilter
matchRelPaths = any matchRelPath

hasLink :: Path Rel File -> DocFilter
hasLink p = astFilter $ maybe False $ elem (Just p) . map (parseRelFile . T.unpack . fst) . findLinks
