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
import Model.MarkdownAst.Classes.HasListItems
import Model.MarkdownAst.Classes.HasListSpacing
import Model.MarkdownAst.Classes.HasListType

data ListParams bl where
  ListData ::
    { _listType :: ListType,
      _listSpacing :: ListSpacing,
      _listItems :: [bl]
    } ->
    ListParams bl
  deriving (Show, Eq)

makeLenses ''ListParams

instance HasListSpacing (ListParams bl) where
  listSpacing = Model.MarkdownAst.Params.ListParams.listSpacing

instance HasListType (ListParams bl) where
  listType = Model.MarkdownAst.Params.ListParams.listType

instance HasListItems ListParams bl where
  listItems = Model.MarkdownAst.Params.ListParams.listItems
