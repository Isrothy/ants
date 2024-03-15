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
import Model.MarkdownAst.Classes.HasColAlignments
import Model.MarkdownAst.Classes.HasHeaders
import Model.MarkdownAst.Classes.HasRows

data PipeTableParams il where
  PipeTableParams ::
    { _colAlignments :: [ColAlignment],
      _headers :: [il],
      _rows :: [[il]]
    } ->
    PipeTableParams il

makeLenses ''PipeTableParams

instance HasColAlignments (PipeTableParams il) where
  colAlignments = Model.MarkdownAst.Params.PipeTableParams.colAlignments

instance HasHeaders PipeTableParams il where
  headers = Model.MarkdownAst.Params.PipeTableParams.headers

instance HasRows PipeTableParams il where
  rows = Model.MarkdownAst.Params.PipeTableParams.rows
