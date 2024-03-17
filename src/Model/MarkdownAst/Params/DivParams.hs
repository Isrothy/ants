{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.DivParams
  ( DivParams (..),
  )
where

import Commonmark.Types
import Control.Lens (makeLenses)
import Model.MarkdownAst.Lenses.HasBlock

data DivParams il bl where
  DivParams ::
    { _block :: bl
    } ->
    DivParams il bl
  deriving (Show, Eq)

makeLenses ''DivParams

instance (IsInline il, IsBlock il bl) => HasBlock DivParams il bl where
  block = Model.MarkdownAst.Params.DivParams.block
