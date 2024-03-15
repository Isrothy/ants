{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.BlockquoteParams
  ( BlockquoteParams (..),
  )
where

import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Classes.HasText

data BlockquoteParams where
  BlockquoteParams ::
    { _text :: T.Text
    } ->
    BlockquoteParams
  deriving (Show, Eq)

makeLenses ''BlockquoteParams

instance HasText BlockquoteParams where
  text = Model.MarkdownAst.Params.BlockquoteParams.text
