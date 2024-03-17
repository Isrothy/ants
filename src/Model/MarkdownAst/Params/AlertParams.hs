{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.AlertParams
  ( AlertParams (..),
  )
where

import Commonmark
import Commonmark.Extensions
import Control.Lens
import Model.MarkdownAst.Lenses.HasAlertType
import Model.MarkdownAst.Lenses.HasBlock

data AlertParams il bl where
  AlertParams ::
    { _alertType :: AlertType,
      _block :: bl
    } ->
    AlertParams il bl
  deriving (Show, Eq)

makeLenses ''AlertParams

instance (IsBlock il bl, IsInline il) => HasBlock AlertParams il bl where
  block = Model.MarkdownAst.Params.AlertParams.block

instance HasAlertType (AlertParams il bl) where
  alertType = Model.MarkdownAst.Params.AlertParams.alertType
