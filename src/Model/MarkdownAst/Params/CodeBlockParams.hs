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
import Model.MarkdownAst.Classes.HasLanguage
import Model.MarkdownAst.Classes.HasText

data CodeBlockParams where
  CodeBlockParams ::
    { _language :: T.Text,
      _text :: T.Text
    } ->
    CodeBlockParams

makeLenses ''CodeBlockParams

instance HasText CodeBlockParams where
  text = Model.MarkdownAst.Params.CodeBlockParams.text

instance HasLanguage CodeBlockParams where
  language = Model.MarkdownAst.Params.CodeBlockParams.language
