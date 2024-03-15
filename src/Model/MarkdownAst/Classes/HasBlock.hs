{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model.MarkdownAst.Classes.HasBlock
  ( HasBlock (..),
  )
where

import Control.Lens (Lens')
import Data.Kind

class HasBlock (a :: Type -> Type) b where
  block :: Lens' (a b) b
