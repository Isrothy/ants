module Model.MarkdownAst.Lenses.HasColAlignments
  ( HasColAlignments (..),
  )
where

import Commonmark.Extensions (ColAlignment)
import Control.Lens (Lens')

class HasColAlignments a where
  colAlignments :: Lens' a [ColAlignment]
