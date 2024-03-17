{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StarIsType #-}

module Model.MarkdownAst.Lenses.HasHeaders
  ( HasHeaders (..),
  )
where

import Commonmark (IsInline)
import Control.Lens (Lens')
import Data.Kind

class (IsInline i) => HasHeaders (a :: Type -> Type) i where
  headers :: Lens' (a i) [i]
