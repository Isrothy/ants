{-# LANGUAGE GADTs #-}

module Model.DocQuery.BoolExpr
  ( BoolExpr (..),
  )
where

data BoolExpr a where
  Val :: a -> BoolExpr a
  And :: (BoolExpr a) -> (BoolExpr a) -> BoolExpr a
  Or :: (BoolExpr a) -> (BoolExpr a) -> BoolExpr a
  Not :: (BoolExpr a) -> BoolExpr a
  deriving (Show, Eq)
