module Model.DocQuery
  ( Query (..),
    Term (..),
    BoolExpr (..),
    match,
    query,
  )
where

import Model.DocQuery.BoolExpr
import Model.DocQuery.Query
import Model.DocQuery.Term
