{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.LinkParams
  ( LinkParams (..),
  )
where

import Commonmark
import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Lenses.HasInline
import Model.MarkdownAst.Lenses.HasTarget
import Model.MarkdownAst.Lenses.HasTitle

data LinkParams il where
  LinkParams ::
    { _target :: T.Text,
      _title :: T.Text,
      _inline :: il
    } ->
    LinkParams il
  deriving (Show, Eq)

makeLenses ''LinkParams

instance (IsInline il) => HasInline LinkParams il where
  inline = Model.MarkdownAst.Params.LinkParams.inline

instance HasTarget (LinkParams il) where
  target = Model.MarkdownAst.Params.LinkParams.target

instance HasTitle (LinkParams il) where
  title = Model.MarkdownAst.Params.LinkParams.title
