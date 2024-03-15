{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.LineBreakParams where

import Control.Lens

data LineBreakParams where
  LineBreakParams ::
    {} ->
    LineBreakParams
  deriving (Show, Eq)

makeLenses ''LineBreakParams
