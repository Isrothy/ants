{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.SoftBreakParams where

import Control.Lens (makeLenses)

data SoftBreakParams where
  LineBreakParams ::
    {} ->
    SoftBreakParams
  deriving (Show, Eq)

makeLenses ''SoftBreakParams
