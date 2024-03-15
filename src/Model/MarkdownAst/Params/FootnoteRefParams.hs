{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.FootnoteRefParams
  ( FootnoteRefParams (..),
    ref,
  )
where

import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Classes.HasLabel
import Model.MarkdownAst.Classes.HasText

data FootnoteRefParams bl where
  FootnoteRefParams ::
    { _label :: T.Text,
      _text :: T.Text,
      _ref :: bl
    } ->
    FootnoteRefParams bl
  deriving (Show, Eq)

makeLenses ''FootnoteRefParams

instance HasLabel (FootnoteRefParams bl) where
  label = Model.MarkdownAst.Params.FootnoteRefParams.label

instance HasText (FootnoteRefParams bl) where
  text = Model.MarkdownAst.Params.FootnoteRefParams.text
