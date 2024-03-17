{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.WikiLinkParams
  ( WikiLinkParams (..),
  )
where

import Commonmark (IsInline)
import Control.Lens (makeLenses)
import Data.Text qualified as T
import Model.MarkdownAst.Lenses.HasInline
import Model.MarkdownAst.Lenses.HasTarget

data WikiLinkParams il where
  WikiLinkParams ::
    { _target :: T.Text,
      _inline :: il
    } ->
    WikiLinkParams il
  deriving (Show, Eq)

makeLenses ''WikiLinkParams

instance (IsInline il) => HasInline WikiLinkParams il where
  inline = Model.MarkdownAst.Params.WikiLinkParams.inline

instance HasTarget (WikiLinkParams il) where
  target = Model.MarkdownAst.Params.WikiLinkParams.target
