{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.SingleQuotedParams
  ( SingleQuotedParams (..),
  )
where

import Control.Lens (makeLenses)
import Model.MarkdownAst.Classes.HasInline

data SingleQuotedParams il where
  SingleQuotedParams ::
    { _text :: il
    } ->
    SingleQuotedParams il

makeLenses ''SingleQuotedParams

instance HasInline SingleQuotedParams il where
  inline = Model.MarkdownAst.Params.SingleQuotedParams.text
