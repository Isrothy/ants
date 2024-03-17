{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.SuperscriptParams
  ( SuperscriptParams (..),
  )
where

import Commonmark
import Control.Lens
import Model.MarkdownAst.Lenses.HasInline

data SuperscriptParams il where
  SuperscriptParams ::
    { _inline :: il
    } ->
    SuperscriptParams il
  deriving (Show, Eq)

makeLenses ''SuperscriptParams

instance (IsInline il) => HasInline SuperscriptParams il where
  inline = Model.MarkdownAst.Params.SuperscriptParams.inline
