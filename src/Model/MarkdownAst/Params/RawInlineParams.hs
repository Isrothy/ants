{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.RawInlineParams
  ( RawInlineParams (..),
  )
where

import Commonmark (Format)
import Control.Lens (makeLenses)
import Data.Text qualified as T
import Model.MarkdownAst.Classes.HasFormat
import Model.MarkdownAst.Classes.HasText

data RawInlineParams where
  RawInlineParams ::
    { _format :: Format,
      _text :: T.Text
    } ->
    RawInlineParams

makeLenses ''RawInlineParams

instance HasText RawInlineParams where
  text = Model.MarkdownAst.Params.RawInlineParams.text

instance HasFormat RawInlineParams where
  format = Model.MarkdownAst.Params.RawInlineParams.format
