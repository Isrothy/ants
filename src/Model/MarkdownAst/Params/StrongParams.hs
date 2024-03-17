{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.StrongParams
  ( StrongParams (..),
  )
where

import Commonmark (IsInline)
import Control.Lens
import Model.MarkdownAst.Lenses.HasInline

data StrongParams il where
  StrongParams ::
    { _inline :: il
    } ->
    StrongParams il
  deriving (Show, Eq)

makeLenses ''StrongParams

instance (IsInline il) => HasInline StrongParams il where
  inline = Model.MarkdownAst.Params.StrongParams.inline
