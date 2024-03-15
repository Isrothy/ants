{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.FootnoteListParams
  ( FootnoteListParams (..),
  )
where

import Control.Lens
import Model.MarkdownAst.Classes.HasListItems

data FootnoteListParams bl where
  FootnoteListParams ::
    { _listItems :: [bl]
    } ->
    FootnoteListParams bl
  deriving (Show, Eq)

makeLenses ''FootnoteListParams

instance HasListItems FootnoteListParams bl where
  listItems = Model.MarkdownAst.Params.FootnoteListParams.listItems
