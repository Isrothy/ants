{-# LANGUAGE GADTs #-}

module Model.DocQuery.Term
  ( Term (..),
    match,
  )
where

import Commonmark.Extensions (AlertType)
import Data.Algebra.Boolean
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Path
import Text.Regex.TDFA hiding (match)
import qualified Util.Fuzzy as Fuzzy
import Prelude hiding (and, any, not, or, (&&), (||))

data Term where
  StrictTerm :: T.Text -> Term
  FuzzyTerm :: T.Text -> Term
  CaseInsensitiveTerm :: T.Text -> Term
  RegexTerm :: T.Text -> Term
  Or :: Term -> Term -> Term
  And :: Term -> Term -> Term
  Not :: Term -> Term
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
  InDirectory :: Path Rel Dir -> Query
  deriving (Show, Eq)

match :: Term -> T.Text -> Bool
match (StrictTerm t) text = t `T.isInfixOf` text
match (FuzzyTerm t) text = t `Fuzzy.isInfixOfT` text
match (CaseInsensitiveTerm t) text = T.isInfixOf (T.toCaseFold t) (T.toCaseFold text)
match (RegexTerm t) text = text =~ t
match (Not t) text = not $ match t text
match (And t1 t2) text = match t1 text && match t2 text
match (Or t1 t2) text = match t1 text || match t2 text
