{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.StrikethroughParams
  ( StrikethroughParams (..),
  )
where

import Commonmark
import Control.Lens (makeLenses)
import Model.MarkdownAst.Lenses.HasInline

data StrikethroughParams il where
  StrikethroughParams ::
    { _inline :: il
    } ->
    StrikethroughParams il
  deriving (Show, Eq)

makeLenses ''StrikethroughParams

instance (IsInline il) => HasInline StrikethroughParams il where
  inline = Model.MarkdownAst.Params.StrikethroughParams.inline
