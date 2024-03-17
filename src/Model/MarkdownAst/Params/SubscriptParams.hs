{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.SubscriptParams
  ( SubscriptParams (..),
  )
where

import Commonmark
import Control.Lens (makeLenses)
import Model.MarkdownAst.Lenses.HasInline

data SubscriptParams il where
  SubscriptParams ::
    { _inline :: il
    } ->
    SubscriptParams il
  deriving (Show, Eq)

makeLenses ''SubscriptParams

instance (IsInline il) => HasInline SubscriptParams il where
  inline = Model.MarkdownAst.Params.SubscriptParams.inline
