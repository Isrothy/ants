{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}

module Model.MarkdownAst.Lenses.HasBlock
  ( HasBlock (..),
  )
where

import Commonmark.Types
import Control.Lens (Lens')
import Data.Kind

class (IsInline i, IsBlock i b) => HasBlock (a :: Type -> Type -> Type) i b where
  block :: Lens' (a i b) b
