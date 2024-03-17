{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.PlainParams
  ( PlainParams (..),
  )
where

import Commonmark
import Control.Lens (makeLenses)
import Model.MarkdownAst.Lenses.HasInline

data PlainParams il where
  PlainParams ::
    { _text :: il
    } ->
    PlainParams il
  deriving (Show, Eq)

makeLenses ''PlainParams

instance (IsInline il) => HasInline PlainParams il where
  inline = Model.MarkdownAst.Params.PlainParams.text
