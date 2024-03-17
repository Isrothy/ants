{-# LANGUAGE ImportQualifiedPost #-}

module Model.MarkdownAst.Lenses.HasLanguage
  ( HasLanguage (..),
  )
where

import Control.Lens (Lens')
import Data.Text qualified as T

class HasLanguage a where
  language :: Lens' a T.Text
