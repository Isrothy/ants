module Model.MarkdownAst.Lenses.HasLevel
  ( HasLevel (..),
  )
where

import Control.Lens (Lens')

class HasLevel a where
  level :: Lens' a Int
