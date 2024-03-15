module Model.MarkdownAst.Classes.HasListType
  ( HasListType (..),
  )
where

import Commonmark.Types
import Control.Lens (Lens')

class HasListType a where
  listType :: Lens' a ListType
