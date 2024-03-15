module Model.MarkdownAst.Classes.HasChar
  ( HasChar (..),
  )
where

import Control.Lens (Lens')

class HasChar a where
  char :: Lens' a Char
