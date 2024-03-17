{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.DocQuery.Query
  ( Query (..),
    query,
    TaskType (..),
  )
where

import Commonmark hiding (plain)
import Commonmark.Extensions (AlertType)
import Control.Lens ((^.))
import Data.Algebra.Boolean
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Model.DocQuery.BoolExpr
import Model.DocQuery.Term
import qualified Model.Document as D
import Model.MarkdownAst hiding (Alert)
import Model.MarkdownAst.Lenses
import Model.MarkdownAst.Params.AlertParams
import qualified Model.Metadata as M
import Path
import Prelude hiding (and, any, not, or, (&&), (||))

type Filter a = a -> Bool

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

plain :: TextFilter -> AstFilter
plain f = maybe False (f . toPlainText)

data TaskType = Done | Todo | Both
  deriving (Show, Eq)

class IsQuery a where
  query :: a -> DocFilter

data Query where
  Author :: (BoolExpr Term) -> Query
  Title :: (BoolExpr Term) -> Query
  Tag :: (BoolExpr Term) -> Query
  Description :: (BoolExpr Term) -> Query
  Content :: (BoolExpr Term) -> Query
  Task :: TaskType -> (BoolExpr Term) -> Query
  Alert :: AlertType -> (BoolExpr Term) -> Query
  DateTimeRange :: Maybe UTCTime -> Maybe UTCTime -> Query
  HasLink :: Path Rel File -> Query
  InDirectory :: Path Rel Dir -> Query
  deriving (Show, Eq)

instance IsQuery Query where
  query (Author t) = metadata $ match t . fromMaybe "" . M.author
  query (Title t) = metadata $ match t . fromMaybe "" . M.title
  query (Tag t) = metadata $ any (match t) . M.tags
  query (Description t) = metadata $ match t . M.description
  query (Content t) = (ast . plain) (match t)
  query (Task Both t) = ast' $ \md ->
    any (match t . toPlainText . snd) $
      findTasks md >>= (^. paramaters . taskListItems)
  query (Task Done t) = ast' $ \md ->
    any (match t . toPlainText . snd) $
      filter fst $
        findTasks md >>= (^. paramaters . taskListItems)
  query (Task Todo t) = ast' $ \md ->
    any (match t . toPlainText . snd) $
      filter (not . fst) $
        findTasks md >>= (^. paramaters . taskListItems)
  query (Alert a t) =
    ast' $ \md ->
      any
        (\(AstNode (AlertParams at b) _ _) -> (at == a) && (match t . toPlainText) b)
        (findAlerts md)
  query (DateTimeRange start end) = metadata $ maybe False (between start end) . M.dateTime
    where
      between (Just s) (Just e) d = s <= e && d >= s && d < e
      between (Just s) _ d = d >= s
      between _ (Just e) d = d < e
      between _ _ _ = True
  query (HasLink p) = ast' $ elem (Just p) . map (parseRelFile . T.unpack . (^. paramaters . title)) . findLinks
  query (InDirectory p) = relPath $ isProperPrefixOf p

instance IsQuery (BoolExpr Query) where
  query (Val q) = query q
  query (And a b) = query a && query b
  query (Or a b) = query a || query b
  query (Not a) = not . query a
