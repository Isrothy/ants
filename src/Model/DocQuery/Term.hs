{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Model.DocQuery.Term
  ( Term (..),
    match,
  )
where

import Data.Algebra.Boolean
import qualified Data.Text as T
import Model.DocQuery.BoolExpr
import Text.Regex.TDFA hiding (match)
import qualified Util.Fuzzy as Fuzzy
import Prelude hiding (and, any, not, or, (&&), (||))

data Term where
  StrictTerm :: T.Text -> Term
  FuzzyTerm :: T.Text -> Term
  CaseInsensitiveTerm :: T.Text -> Term
  RegexTerm :: T.Text -> Term
  deriving (Show, Eq)

class IsTerm a where
  match :: a -> T.Text -> Bool

instance IsTerm Term where
  match (StrictTerm t) text = t `T.isInfixOf` text
  match (FuzzyTerm t) text = t `Fuzzy.isInfixOfT` text
  match (CaseInsensitiveTerm t) text = T.isInfixOf (T.toCaseFold t) (T.toCaseFold text)
  match (RegexTerm t) text = text =~ t

instance IsTerm (BoolExpr Term) where
  match (Val t) text = match t text
  match (Not t) text = not $ match t text
  match (And t1 t2) text = match t1 text && match t2 text
  match (Or t1 t2) text = match t1 text || match t2 text
