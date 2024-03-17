{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.BlockquoteParams
  ( BlockquoteParams (..),
  )
where

import Commonmark
import Control.Lens
import Model.MarkdownAst.Lenses.HasBlock

data BlockquoteParams il bl where
  BlockquoteParams ::
    { _block :: bl
    } ->
    BlockquoteParams il bl
  deriving (Show, Eq)

makeLenses ''BlockquoteParams

instance (IsInline il, IsBlock il bl) => HasBlock BlockquoteParams il bl where
  block = Model.MarkdownAst.Params.BlockquoteParams.block
