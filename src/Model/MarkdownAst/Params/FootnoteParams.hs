{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.FootnoteParams
  ( FootnoteParams (..),
  )
where

import Commonmark.Types
import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Lenses.HasBlock
import Model.MarkdownAst.Lenses.HasLabel
import Model.MarkdownAst.Lenses.HasNumber

data FootnoteParams il bl where
  FootnoteParams ::
    { _number :: Int,
      _label :: T.Text,
      _block :: bl
    } ->
    FootnoteParams il bl
  deriving (Show, Eq)

makeLenses ''FootnoteParams

instance (IsInline il, IsBlock il bl) => HasBlock FootnoteParams il bl where
  block = Model.MarkdownAst.Params.FootnoteParams.block

instance HasLabel (FootnoteParams il bl) where
  label = Model.MarkdownAst.Params.FootnoteParams.label

instance HasNumber (FootnoteParams il bl) where
  number = Model.MarkdownAst.Params.FootnoteParams.number
