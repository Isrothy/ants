{-# LANGUAGE ImportQualifiedPost #-}

module Model.MarkdownAst.Lenses.HasAscii
  ( HasAscii (..),
  )
where

import Control.Lens (Lens')
import Data.Text qualified as T

class HasAscii a where
  ascii :: Lens' a T.Text
