{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.ListParams
  ( ListParams (..),
  )
where

import Commonmark.Types
import Control.Lens
import Model.MarkdownAst.Lenses.HasListItems
import Model.MarkdownAst.Lenses.HasListSpacing
import Model.MarkdownAst.Lenses.HasListType

data ListParams il bl where
  ListData ::
    { _listType :: ListType,
      _listSpacing :: ListSpacing,
      _listItems :: [bl]
    } ->
    ListParams il bl
  deriving (Show, Eq)

makeLenses ''ListParams

instance HasListSpacing (ListParams il bl) where
  listSpacing = Model.MarkdownAst.Params.ListParams.listSpacing

instance HasListType (ListParams il bl) where
  listType = Model.MarkdownAst.Params.ListParams.listType

instance (IsInline il, IsBlock il bl) => HasListItems ListParams il bl where
  listItems = Model.MarkdownAst.Params.ListParams.listItems
