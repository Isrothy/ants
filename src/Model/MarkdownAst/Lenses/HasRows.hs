{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StarIsType #-}

module Model.MarkdownAst.Lenses.HasRows
  ( HasRows (..),
  )
where

import Control.Lens (Lens')
import Data.Kind
import Commonmark

class (IsInline i) => HasRows (a :: Type -> Type) i where
  rows :: Lens' (a i) [[i]]
