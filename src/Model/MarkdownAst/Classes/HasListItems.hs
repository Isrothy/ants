{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model.MarkdownAst.Classes.HasListItems
  ( HasListItems (..),
  )
where

import Control.Lens (Lens')
import Data.Kind

class HasListItems (a :: Type -> Type) b where
  listItems :: Lens' (a b) [b]
