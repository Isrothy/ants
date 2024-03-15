{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.AlertParams
  ( AlertParams (..),
  )
where

import Commonmark.Extensions
import Control.Lens
import Model.MarkdownAst.Classes.HasAlertType
import Model.MarkdownAst.Classes.HasBlock

data AlertParams bl where
  AlertParams ::
    { _alertType :: AlertType,
      _block :: bl
    } ->
    AlertParams bl
  deriving (Show, Eq)

makeLenses ''AlertParams

instance HasBlock AlertParams bl where
  block = Model.MarkdownAst.Params.AlertParams.block

instance HasAlertType (AlertParams bl) where
  alertType = Model.MarkdownAst.Params.AlertParams.alertType
