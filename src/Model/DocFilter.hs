{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Model.DocFilter
  ( Filter (..),
    TextFilter,
    AstFilter,
    DocFilter,
    dateRange,
    author,
    title,
    tag,
    description,
    fuzzyTerm,
    strictTerm,
    caseInsensitiveTerm,
    regexTerm,
    matchesRelPath,
    hasLink,
    plain,
    content,
    task,
    finishedTask,
    unfinishedTask,
    hasAlert,
    hasAlertType,
  )
where

import Commonmark hiding (plain)
import Commonmark.Extensions
import Data.Algebra.Boolean
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import qualified Model.Document as D
import Model.MarkdownAst
import qualified Model.Metadata as M
import qualified Model.SearchLang as L
import Path
import Text.Regex.TDFA
import qualified Util.Fuzzy as Fuzzy
import Prelude hiding (and, any, not, or, (&&), (||))

type Filter a = a -> Bool

-- instance Boolean (Filter a) where
--   true = Filter $ const True
--   false = Filter $ const False
--   (&&) f1 f2 = Filter $ \x -> filt f1 x && filt f2 x
--   (||) f1 f2 = Filter $ \x -> filt f1 x || filt f2 x
--   not f = Filter $ \x -> not $ filt f x
--
type DocFilter = Filter D.Document

type AstFilter = Filter (Maybe MarkdownAst)

type TextFilter = Filter T.Text

metadata :: (M.Metadata -> Bool) -> DocFilter
metadata f = f . D.metadata

ast :: AstFilter -> DocFilter
ast f = f . D.ast

ast' :: (MarkdownAst -> Bool) -> DocFilter
ast' = ast . maybe False

relPath :: (Path Rel File -> Bool) -> DocFilter
relPath f = f . D.relPath

fuzzyTerm :: T.Text -> TextFilter
fuzzyTerm = Fuzzy.isInfixOfT

strictTerm :: T.Text -> TextFilter
strictTerm = T.isInfixOf

caseInsensitiveTerm :: T.Text -> TextFilter
caseInsensitiveTerm t = T.isInfixOf (T.toCaseFold t) . T.toCaseFold

regexTerm :: T.Text -> TextFilter
regexTerm s = (=~ s)

matchesRelPath :: Path Rel File -> DocFilter
matchesRelPath = relPath . (==)

plain :: TextFilter -> AstFilter
plain f = f . toPlainText

dateRange :: Maybe UTCTime -> Maybe UTCTime -> DocFilter
dateRange start end = metadata $ maybe False (between start end) . M.dateTime
  where
    between (Just s) (Just e) d = s <= e && d >= s && d <= e
    between (Just s) _ d = d >= s
    between _ (Just e) d = d <= e
    between _ _ _ = True

author :: TextFilter -> DocFilter
author f = metadata $ f . fromMaybe "" . M.author

title :: TextFilter -> DocFilter
title f = metadata $ f . fromMaybe "" . M.title

tag :: TextFilter -> DocFilter
tag f = metadata $ any f . M.tags

description :: TextFilter -> DocFilter
description f = metadata $ f . M.description

content :: TextFilter -> DocFilter
content = ast . plain

task :: TextFilter -> DocFilter
task f = ast' $ any (f . toPlainText . snd) . findTasks

finishedTask :: TextFilter -> DocFilter
finishedTask f = ast' $ any (f . toPlainText) . findFinishedTasks

unfinishedTask :: TextFilter -> DocFilter
unfinishedTask f = ast' $ any (f . toPlainText) . findUnfinishedTasks

hasAlert :: DocFilter
hasAlert = ast' $ not . null . findAlerts

hasAlertType :: AlertType -> DocFilter
hasAlertType t = ast' $ any ((== t) . fst) . findAlerts

hasLink :: Path Rel File -> DocFilter
hasLink p = ast' $ elem (Just p) . map (parseRelFile . T.unpack . fst) . findLinks
