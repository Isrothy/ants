{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.HighlightParams
  ( HighlightParams (..),
  )
where

import Commonmark (IsInline)
import Control.Lens
import Model.MarkdownAst.Lenses.HasInline

data HighlightParams il where
  HighlightParams ::
    { _inline :: il
    } ->
    HighlightParams il
  deriving (Show, Eq)

makeLenses ''HighlightParams

instance (IsInline il) => HasInline HighlightParams il where
  inline = Model.MarkdownAst.Params.HighlightParams.inline
