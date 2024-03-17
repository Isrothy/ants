{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model.MarkdownAst.Lenses.HasTaskListItems
  ( HasTaskListItems (..),
  )
where

import Commonmark
import Control.Lens (Lens')
import Data.Kind

class (IsInline i, IsBlock i b) => HasTaskListItems (a :: Type -> Type -> Type) i b where
  taskListItems :: Lens' (a i b) [(Bool, b)]
