{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.PipeTableParams
  ( PipeTableParams (..),
  )
where

import Commonmark.Extensions (ColAlignment)
import Control.Lens
import Model.MarkdownAst.Lenses.HasColAlignments
import Model.MarkdownAst.Lenses.HasHeaders
import Model.MarkdownAst.Lenses.HasRows
import Commonmark

data PipeTableParams il where
  PipeTableParams ::
    { _colAlignments :: [ColAlignment],
      _headers :: [il],
      _rows :: [[il]]
    } ->
    PipeTableParams il
  deriving (Show, Eq)

makeLenses ''PipeTableParams

instance HasColAlignments (PipeTableParams il) where
  colAlignments = Model.MarkdownAst.Params.PipeTableParams.colAlignments

instance (IsInline il) => HasHeaders PipeTableParams il where
  headers = Model.MarkdownAst.Params.PipeTableParams.headers

instance (IsInline il) => HasRows PipeTableParams il where
  rows = Model.MarkdownAst.Params.PipeTableParams.rows
