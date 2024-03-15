{-# LANGUAGE ImportQualifiedPost #-}

module Model.MarkdownAst.Classes.HasText
  ( HasText (..),
  )
where

import Control.Lens (Lens')
import Data.Text qualified as T

class HasText a where
  text :: Lens' a T.Text
