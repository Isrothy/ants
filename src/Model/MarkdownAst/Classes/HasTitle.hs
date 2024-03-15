{-# LANGUAGE FunctionalDependencies #-}

module Model.MarkdownAst.Classes.HasTitle
  ( HasTitle (..),
  )
where

import Control.Lens (Lens')
import qualified Data.Text as T

class HasTitle a where
  title :: Lens' a T.Text
