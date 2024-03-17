{-# LANGUAGE ImportQualifiedPost #-}

module Model.MarkdownAst.Lenses.HasText
  ( HasText (..),
  )
where

import Control.Lens (Lens')
import Data.Text qualified as T

class HasText a where
  text :: Lens' a T.Text
