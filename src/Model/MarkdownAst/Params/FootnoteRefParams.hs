{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.FootnoteRefParams
  ( FootnoteRefParams (..),
  )
where

import Commonmark
import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Lenses.HasBlock
import Model.MarkdownAst.Lenses.HasHref
import Model.MarkdownAst.Lenses.HasLabel

data FootnoteRefParams il bl where
  FootnoteRefParams ::
    { _href :: T.Text,
      _label :: T.Text,
      _block :: bl
    } ->
    FootnoteRefParams il bl
  deriving (Show, Eq)

makeLenses ''FootnoteRefParams

instance HasLabel (FootnoteRefParams il bl) where
  label = Model.MarkdownAst.Params.FootnoteRefParams.label

instance HasHref (FootnoteRefParams il bl) where
  href = Model.MarkdownAst.Params.FootnoteRefParams.href

instance (IsInline il, IsBlock il bl) => HasBlock FootnoteRefParams il bl where
  block = Model.MarkdownAst.Params.FootnoteRefParams.block
