{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.TextParams
  ( TextParams (..),
  )
where

import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Lenses.HasText

data TextParams where
  TextParams ::
    { _text :: T.Text
    } ->
    TextParams
  deriving (Show, Eq)

makeLenses ''TextParams

instance HasText TextParams where
  text = Model.MarkdownAst.Params.TextParams.text
