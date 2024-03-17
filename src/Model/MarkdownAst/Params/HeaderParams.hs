{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.HeaderParams
  ( HeaderParams (..),
  )
where

import Commonmark.Types
import Control.Lens
import Model.MarkdownAst.Lenses.HasInline
import Model.MarkdownAst.Lenses.HasLevel

data HeaderParams il where
  HeaderParams ::
    { _level :: Int,
      _inline :: il
    } ->
    HeaderParams il
  deriving (Show, Eq)

makeLenses ''HeaderParams

instance (IsInline il) => HasInline HeaderParams il where
  inline = Model.MarkdownAst.Params.HeaderParams.inline

instance HasLevel (HeaderParams il) where
  level = Model.MarkdownAst.Params.HeaderParams.level
