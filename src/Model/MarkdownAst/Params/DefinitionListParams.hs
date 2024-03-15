{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.DefinitionListParams
  ( DefinitionListParams (..),
  )
where

import Commonmark (ListSpacing)
import Control.Lens
import Model.MarkdownAst.Classes.HasDefListItems
import Model.MarkdownAst.Classes.HasListSpacing

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

instance HasDefListItems DefinitionListParams il bl where
  defListItems = Model.MarkdownAst.Params.DefinitionListParams.defListItems
