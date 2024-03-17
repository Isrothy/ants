module Model.MarkdownAst.Lenses.HasFormat
  ( HasFormat (..),
  )
where

import Commonmark (Format)
import Control.Lens (Lens')

class HasFormat a where
  format :: Lens' a Format
