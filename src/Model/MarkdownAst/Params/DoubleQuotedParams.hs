{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.DoubleQuotedParams
  ( DoubleQuotedParams (..),
  )
where

import Commonmark (IsInline)
import Control.Lens (makeLenses)
import Model.MarkdownAst.Lenses.HasInline (HasInline (..))

data DoubleQuotedParams il where
  DoubleQuotedParams ::
    { _inline :: il
    } ->
    DoubleQuotedParams il
  deriving (Show, Eq)

makeLenses ''DoubleQuotedParams

instance (IsInline il) => HasInline DoubleQuotedParams il where
  inline = Model.MarkdownAst.Params.DoubleQuotedParams.inline
