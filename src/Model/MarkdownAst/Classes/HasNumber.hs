module Model.MarkdownAst.Classes.HasNumber
  ( HasNumber (..),
  )
where

import Control.Lens (Lens')

class HasNumber a where
  number :: Lens' a Int
