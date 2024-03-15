{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.FootnoteParams
  ( FootnoteParams (..),
  )
where

import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Classes.HasBlock
import Model.MarkdownAst.Classes.HasLabel
import Model.MarkdownAst.Classes.HasNumber

data FootnoteParams bl where
  FootnoteParams ::
    { _number :: Int,
      _label :: T.Text,
      _block :: bl
    } ->
    FootnoteParams bl
  deriving (Show, Eq)

makeLenses ''FootnoteParams

instance HasBlock FootnoteParams bl where
  block = Model.MarkdownAst.Params.FootnoteParams.block

instance HasLabel (FootnoteParams bl) where
  label = Model.MarkdownAst.Params.FootnoteParams.label

instance HasNumber (FootnoteParams bl) where
  number = Model.MarkdownAst.Params.FootnoteParams.number
