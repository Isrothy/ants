module Model.MarkdownAst.Lenses.HasChar

  ( HasChar (..),
  )
where

import Control.Lens (Lens')

class HasChar a where
  char :: Lens' a Char
