{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.DocQuery.Query
  ( Query (..),
    query,
  )
where

import Commonmark hiding (plain)
import Commonmark.Extensions (AlertType)
import Data.Algebra.Boolean
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Model.DocQuery.Term
import qualified Model.Document as D
import Model.MarkdownAst hiding (Alert)
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
plain f = f . toPlainText

data TaskType = Finished | UnFinished | Both
  deriving (Show, Eq)

data Query where
  Author :: Term -> Query
  Title :: Term -> Query
  Tag :: Term -> Query
  Description :: Term -> Query
  Content :: Term -> Query
  Task :: TaskType -> Term -> Query
  Alert :: AlertType -> Query
  DateRange :: Maybe UTCTime -> Maybe UTCTime -> Query
  HasLink :: Path Rel File -> Query
  InDirectory :: Path Rel Dir -> Query
  deriving (Show, Eq)

query :: Query -> DocFilter
query (Author t) = metadata $ match t . fromMaybe "" . M.author
query (Title t) = metadata $ match t . fromMaybe "" . M.title
query (Tag t) = metadata $ any (match t) . M.tags
query (Description t) = metadata $ match t . M.description
query (Content t) = (ast . plain) (match t)
query (Task Both t) = ast' $ any (match t . toPlainText . snd) . findTasks
query (Task Finished t) = ast' $ any (match t . toPlainText) . findFinishedTasks
query (Task UnFinished t) = ast' $ any (match t . toPlainText) . findUnfinishedTasks
query (Alert a) = ast' $ any ((== a) . fst) . findAlerts
query (DateRange start end) = metadata $ maybe False (between start end) . M.dateTime
  where
    between (Just s) (Just e) d = s <= e && d >= s && d <= e
    between (Just s) _ d = d >= s
    between _ (Just e) d = d <= e
    between _ _ _ = True
query (HasLink p) = ast' $ elem (Just p) . map (parseRelFile . T.unpack . fst) . findLinks
query (InDirectory p) = relPath $ isProperPrefixOf p
