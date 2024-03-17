{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.CodeBlockParams
  ( CodeBlockParams (..),
  )
where

import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Lenses.HasLanguage
import Model.MarkdownAst.Lenses.HasText

data CodeBlockParams where
  CodeBlockParams ::
    { _language :: T.Text,
      _text :: T.Text
    } ->
    CodeBlockParams
  deriving (Show, Eq)

makeLenses ''CodeBlockParams

instance HasText CodeBlockParams where
  text = Model.MarkdownAst.Params.CodeBlockParams.text

instance HasLanguage CodeBlockParams where
  language = Model.MarkdownAst.Params.CodeBlockParams.language
