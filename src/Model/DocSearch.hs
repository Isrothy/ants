{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.DocSearch
  ( Term,
    searchText,
    query,
  )
where

import Commonmark hiding (plain)
import Commonmark.Extensions (AlertType)
import Data.Algebra.Boolean
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import qualified Model.Document as D
import Model.MarkdownAst hiding (Alert)
import qualified Model.Metadata as M
import Path
import Text.Regex.TDFA
import qualified Util.Fuzzy as Fuzzy
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

data Term where
  StrictTerm :: T.Text -> Term
  FuzzyTerm :: T.Text -> Term
  CaseInsensitiveTerm :: T.Text -> Term
  RegexTerm :: T.Text -> Term
  Not :: Term -> Term
  And :: Term -> Term -> Term
  Or :: Term -> Term -> Term
  deriving (Show, Eq)

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
  deriving (Show, Eq)

searchText :: Term -> TextFilter
searchText (StrictTerm t) text = t `T.isInfixOf` text
searchText (FuzzyTerm t) text = t `Fuzzy.isInfixOfT` text
searchText (CaseInsensitiveTerm t) text = T.isInfixOf (T.toCaseFold t) (T.toCaseFold text)
searchText (RegexTerm t) text = text =~ t
searchText (Not t) text = not $ searchText t text
searchText (And t1 t2) text = searchText t1 text && searchText t2 text
searchText (Or t1 t2) text = searchText t1 text || searchText t2 text

query :: Query -> DocFilter
query (Author t) = metadata $ searchText t . fromMaybe "" . M.author
query (Title t) = metadata $ searchText t . fromMaybe "" . M.title
query (Tag t) = metadata $ any (searchText t) . M.tags
query (Description t) = metadata $ searchText t . M.description
query (Content t) = (ast . plain) (searchText t)
query (Task Both t) = ast' $ any (searchText t . toPlainText . snd) . findTasks
query (Task Finished t) = ast' $ any (searchText t . toPlainText) . findFinishedTasks
query (Task UnFinished t) = ast' $ any (searchText t . toPlainText) . findUnfinishedTasks
query (Alert a) = ast' $ any ((== a) . fst) . findAlerts
query (DateRange start end) = metadata $ maybe False (between start end) . M.dateTime
  where
    between (Just s) (Just e) d = s <= e && d >= s && d <= e
    between (Just s) _ d = d >= s
    between _ (Just e) d = d <= e
    between _ _ _ = True
query (HasLink p) = ast' $ elem (Just p) . map (parseRelFile . T.unpack . fst) . findLinks
