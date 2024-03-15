{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.WikiLinkParams where

import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Classes.HasInline
import Model.MarkdownAst.Classes.HasTarget

data WikiLinkParams il where
  WikiLinkParams ::
    { _target :: T.Text,
      _inline :: il
    } ->
    WikiLinkParams il
  deriving (Show, Eq)

instance HasInline WikiLinkParams il where
  inline = inline

instance HasTarget (WikiLinkParams il) where
  target = target

makeLenses ''WikiLinkParams
