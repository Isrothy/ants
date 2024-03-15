{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model.MarkdownAst.Classes.HasDefListItems
  ( HasDefListItems (..),
  )
where

import Control.Lens (Lens')
import Data.Kind

class HasDefListItems (a :: Type -> Type -> Type) il bl where
  defListItems :: Lens' (a il bl) [(il, [bl])]
