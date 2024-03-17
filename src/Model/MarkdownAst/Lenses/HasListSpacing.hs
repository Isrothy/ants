{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StarIsType #-}

module Model.MarkdownAst.Lenses.HasListSpacing
  ( HasListSpacing (..),
  )
where

import Commonmark (ListSpacing)
import Control.Lens (Lens')

class HasListSpacing a where
  listSpacing :: Lens' a ListSpacing
