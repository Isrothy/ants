{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.TextParams where

import Control.Lens
import Data.Text qualified as T

data TextParams where
  TextParams ::
    { _text :: T.Text
    } ->
    TextParams
  deriving (Show, Eq)

makeLenses ''TextParams
