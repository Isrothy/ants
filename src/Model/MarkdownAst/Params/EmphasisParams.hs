{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.EmphasisParams
  ( EmphasisParams (..),
  )
where

import Commonmark (IsInline)
import Control.Lens (makeLenses)
import Model.MarkdownAst.Lenses.HasInline

data EmphasisParams il where
  EmphasisParams ::
    { _inline :: il
    } ->
    EmphasisParams il
  deriving (Show, Eq)

makeLenses ''EmphasisParams

instance (IsInline i) => HasInline EmphasisParams i where
  inline = Model.MarkdownAst.Params.EmphasisParams.inline
