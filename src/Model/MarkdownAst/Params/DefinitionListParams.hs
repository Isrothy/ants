{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.DefinitionListParams
  ( DefinitionListParams (..),
  )
where

import Commonmark.Types
import Control.Lens
import Model.MarkdownAst.Lenses.HasDefListItems
import Model.MarkdownAst.Lenses.HasListSpacing

data DefinitionListParams il bl where
  DefinitionListParams ::
    { _listSpacing :: ListSpacing,
      _defListItems :: [(il, [bl])]
    } ->
    DefinitionListParams il bl
  deriving (Show, Eq)

makeLenses ''DefinitionListParams

instance HasListSpacing (DefinitionListParams il bl) where
  listSpacing = Model.MarkdownAst.Params.DefinitionListParams.listSpacing

instance (IsInline il, IsBlock il bl) => HasDefListItems DefinitionListParams il bl where
  defListItems = Model.MarkdownAst.Params.DefinitionListParams.defListItems
