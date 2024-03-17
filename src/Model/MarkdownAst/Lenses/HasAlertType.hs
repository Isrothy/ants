module Model.MarkdownAst.Lenses.HasAlertType
  ( HasAlertType (..),
  )
where

import Commonmark.Extensions (AlertType)
import Control.Lens (Lens')

class HasAlertType a where
  alertType :: Lens' a AlertType
