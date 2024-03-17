{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StarIsType #-}

module Model.MarkdownAst.Lenses.HasInline
  ( HasInline (..),
  )
where

import Commonmark (IsInline)
import Control.Lens (Lens')
import Data.Kind

class (IsInline i) => HasInline (a :: Type -> Type) i where
  inline :: Lens' (a i) i
