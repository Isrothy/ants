{-# LANGUAGE ImportQualifiedPost #-}

module Model.MarkdownAst.Lenses.HasEmoji
  ( HasEmoji (..),
  )
where

import Control.Lens (Lens')
import Data.Text qualified as T

class HasEmoji a where
  emoji :: Lens' a T.Text
