{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StarIsType #-}

module Model.MarkdownAst.Classes.HasRows
  ( HasRows (..),
  )
where

import Control.Lens (Lens')
import Data.Kind

class HasRows (a :: Type -> Type) i where
  rows :: Lens' (a i) [[i]]
