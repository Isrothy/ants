module Model.MarkdownAst.Classes.HasFormat
  ( HasFormat (..),
  )
where

import Commonmark (Format)
import Control.Lens (Lens')

class HasFormat a where
  format :: Lens' a Format
