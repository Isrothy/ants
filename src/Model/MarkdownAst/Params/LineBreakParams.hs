{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.LineBreakParams where

import Control.Lens
import Data.Text qualified as T

data LineBreakParams where
  LineBreakParams ::
    {} ->
    LineBreakParams
  deriving (Show, Eq)

makeLenses ''LineBreakParams
