{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model.MarkdownAst.Lenses.HasListItems
  ( HasListItems (..),
  )
where

import Commonmark.Types
import Control.Lens (Lens')
import Data.Kind

class (IsInline i, IsBlock i b) => HasListItems (a :: Type -> Type -> Type) i b where
  listItems :: Lens' (a i b) [b]
