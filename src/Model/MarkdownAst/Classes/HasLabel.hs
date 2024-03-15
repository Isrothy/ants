module Model.MarkdownAst.Classes.HasLabel
  ( HasLabel (..),
  )
where

import Control.Lens (Lens')
import qualified Data.Text as T

class HasLabel a where
  label :: Lens' a T.Text
