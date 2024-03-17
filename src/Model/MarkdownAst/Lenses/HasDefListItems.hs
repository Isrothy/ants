{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model.MarkdownAst.Lenses.HasDefListItems
  ( HasDefListItems (..),
  )
where

import Commonmark.Types
import Control.Lens (Lens')
import Data.Kind

class (IsInline il, IsBlock il bl) => HasDefListItems (a :: Type -> Type -> Type) il bl where
  defListItems :: Lens' (a il bl) [(il, [bl])]
