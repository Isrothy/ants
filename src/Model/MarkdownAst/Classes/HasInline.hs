{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StarIsType #-}

module Model.MarkdownAst.Classes.HasInline
  ( HasInline (..),
  )
where

import Control.Lens (Lens')
import Data.Kind

class HasInline (a :: Type -> Type) i where
  inline :: Lens' (a i) i
