module Model.MarkdownAst.Classes.HasTarget
  ( HasTarget (..),
  )
where

import Control.Lens (Lens')
import qualified Data.Text as T

class HasTarget a where
  target :: Lens' a T.Text
