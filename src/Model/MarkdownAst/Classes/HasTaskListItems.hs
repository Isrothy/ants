{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model.MarkdownAst.Classes.HasTaskListItems
  ( HasTaskListItems (..),
  )
where

import Control.Lens (Lens')
import Data.Kind

class HasTaskListItems (a :: Type -> Type) b where
  taskListItems :: Lens' (a b) [(Bool, b)]
