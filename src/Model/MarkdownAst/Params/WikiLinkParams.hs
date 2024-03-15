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

import Control.Lens (makeLenses)
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

makeLenses ''WikiLinkParams

instance HasInline WikiLinkParams il where
  inline = Model.MarkdownAst.Params.WikiLinkParams.inline

instance HasTarget (WikiLinkParams il) where
  target = Model.MarkdownAst.Params.WikiLinkParams.target
