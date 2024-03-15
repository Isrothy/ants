{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.PlainParams
  ( PlainParams (..),
  )
where

import Control.Lens (makeLenses)
import Model.MarkdownAst.Classes.HasInline

data PlainParams il where
  PlainParams ::
    { _text :: il
    } ->
    PlainParams il

makeLenses ''PlainParams

instance HasInline PlainParams il where
  inline = Model.MarkdownAst.Params.PlainParams.text
