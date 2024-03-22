{-# LANGUAGE GADTs #-}

module Model.DocQuery.BoolExpr
  ( BoolExpr (..),
  )
where

import Data.Algebra.Boolean

data BoolExpr a where
  Var :: a -> BoolExpr a
  Const :: Bool -> BoolExpr a
  And :: BoolExpr a -> BoolExpr a -> BoolExpr a
  Or :: BoolExpr a -> BoolExpr a -> BoolExpr a
  Not :: BoolExpr a -> BoolExpr a
  deriving (Show, Eq)

instance Boolean (BoolExpr a) where
  (&&) = And
  (||) = Or
  not = Not
  true = Const True
  false = Const False
