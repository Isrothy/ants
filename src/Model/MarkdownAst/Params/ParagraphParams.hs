{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.ParagraphParams
  ( ParagraphParams (..),
  )
where

import Commonmark.Types
import Control.Lens
import Model.MarkdownAst.Lenses.HasInline

data ParagraphParams il where
  ParagraphParams ::
    { _inline :: il
    } ->
    ParagraphParams il
  deriving (Show, Eq)

makeLenses ''ParagraphParams

instance (IsInline il) => HasInline ParagraphParams il where
  inline = Model.MarkdownAst.Params.ParagraphParams.inline
