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

import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Classes.HasInline
import Model.MarkdownAst.Classes.HasTarget
import Model.MarkdownAst.Classes.HasTitle

data LinkParams il where
  LinkParams ::
    { _target :: T.Text,
      _title :: T.Text,
      _inline :: il
    } ->
    LinkParams il
  deriving (Show, Eq)

makeLenses ''LinkParams

instance HasInline LinkParams il where
  inline = Model.MarkdownAst.Params.LinkParams.inline

instance HasTarget (LinkParams il) where
  target = Model.MarkdownAst.Params.LinkParams.target

instance HasTitle (LinkParams il) where
  title = Model.MarkdownAst.Params.LinkParams.title
