{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.SingleQuotedParams
  ( SingleQuotedParams (..),
  )
where

import Commonmark (IsInline)
import Control.Lens (makeLenses)
import Model.MarkdownAst.Lenses.HasInline

data SingleQuotedParams il where
  SingleQuotedParams ::
    { _text :: il
    } ->
    SingleQuotedParams il
  deriving (Show, Eq)

makeLenses ''SingleQuotedParams

instance (IsInline il) => HasInline SingleQuotedParams il where
  inline = Model.MarkdownAst.Params.SingleQuotedParams.text
