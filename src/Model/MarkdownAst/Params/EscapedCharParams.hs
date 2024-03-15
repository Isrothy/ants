{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.EscapedCharParams
  ( EscapedCharParams (..),
  )
where

import Control.Lens
import Model.MarkdownAst.Classes.HasChar

data EscapedCharParams where
  EscapedCharParams ::
    {_char :: Char} ->
    EscapedCharParams
  deriving (Show, Eq)

makeLenses ''EscapedCharParams

instance HasChar EscapedCharParams where
  char = Model.MarkdownAst.Params.EscapedCharParams.char
