{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.InlineMathParams where

import Control.Lens (makeLenses)
import Data.Text qualified as T
import Model.MarkdownAst.Classes.HasText

data InlineMathParams where
  InlineMathParams ::
    { _text :: T.Text
    } ->
    InlineMathParams

makeLenses ''InlineMathParams

instance HasText InlineMathParams where
  text = Model.MarkdownAst.Params.InlineMathParams.text
