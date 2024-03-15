{-# LANGUAGE ImportQualifiedPost #-}

module Model.MarkdownAst.Classes.HasTitle
  ( HasTitle (..),
  )
where

import Control.Lens (Lens')
import Data.Text qualified as T

class HasTitle a where
  title :: Lens' a T.Text
