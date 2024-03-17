{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.FootnoteListParams
  ( FootnoteListParams (..),
  )
where

import Commonmark
import Control.Lens
import Model.MarkdownAst.Lenses.HasListItems

data FootnoteListParams il bl where
  FootnoteListParams ::
    { _listItems :: [bl]
    } ->
    FootnoteListParams il bl
  deriving (Show, Eq)

makeLenses ''FootnoteListParams

instance (IsInline il, IsBlock il bl) => HasListItems FootnoteListParams il bl where
  listItems = Model.MarkdownAst.Params.FootnoteListParams.listItems
