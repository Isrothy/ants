{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.ImageParams
  ( ImageParams (..),
  )
where

import Commonmark.Types
import Control.Lens (makeLenses)
import Data.Text qualified as T
import Model.MarkdownAst.Lenses.HasInline
import Model.MarkdownAst.Lenses.HasTarget
import Model.MarkdownAst.Lenses.HasTitle

data ImageParams il where
  ImageParams ::
    { _target :: T.Text,
      _title :: T.Text,
      _inline :: il
    } ->
    ImageParams il
  deriving (Show, Eq)

makeLenses ''ImageParams

instance (IsInline il) => HasInline ImageParams il where
  inline = Model.MarkdownAst.Params.ImageParams.inline

instance HasTarget (ImageParams il) where
  target = Model.MarkdownAst.Params.ImageParams.target

instance HasTitle (ImageParams il) where
  title = Model.MarkdownAst.Params.ImageParams.title
