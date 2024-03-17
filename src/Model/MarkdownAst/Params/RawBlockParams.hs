{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.RawBlockParams
  ( RawBlockParams (..),
  )
where

import Commonmark (Format)
import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Lenses.HasFormat
import Model.MarkdownAst.Lenses.HasText

data RawBlockParams where
  RawBlockParams ::
    { _format :: Format,
      _text :: T.Text
    } ->
    RawBlockParams
  deriving (Show, Eq)

makeLenses ''RawBlockParams

instance HasText RawBlockParams where
  text = Model.MarkdownAst.Params.RawBlockParams.text

instance HasFormat RawBlockParams where
  format = Model.MarkdownAst.Params.RawBlockParams.format
