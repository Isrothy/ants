{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.SpanParams
  ( SpanParams (..),
  )
where

import Commonmark.Types
import Control.Lens (makeLenses)
import Model.MarkdownAst.Lenses.HasInline

data SpanParams il where
  SpanParams ::
    { _inline :: il
    } ->
    SpanParams il
  deriving (Show, Eq)

makeLenses ''SpanParams

instance (IsInline il) => HasInline SpanParams il where
  inline = Model.MarkdownAst.Params.SpanParams.inline
