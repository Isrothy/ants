{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StarIsType #-}

module Model.MarkdownAst.Classes.HasHeaders
  ( HasHeaders (..),
  )
where

import Control.Lens (Lens')
import Data.Kind

class HasHeaders (a :: Type -> Type) i where
  headers :: Lens' (a i) [i]
