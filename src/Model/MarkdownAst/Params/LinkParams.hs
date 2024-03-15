{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.LinkParams where

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

instance HasInline LinkParams il where
  inline = inline

instance HasTarget (LinkParams il) where
  target = target

instance HasTitle (LinkParams il) where
  title = title

makeLenses ''LinkParams
