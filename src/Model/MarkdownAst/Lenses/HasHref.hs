{-# LANGUAGE ImportQualifiedPost #-}

module Model.MarkdownAst.Lenses.HasHref
  ( HasHref (..),
  )
where

import Control.Lens (Lens')
import Data.Text qualified as T

class HasHref a where
  href :: Lens' a T.Text
