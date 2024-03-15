{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.EntityParams
  ( EntityParams (..),
  )
where

import Control.Lens (makeLenses)
import Data.Text qualified as T
import Model.MarkdownAst.Classes.HasText

data EntityParams where
  EntityParams ::
    { _entity :: T.Text
    } ->
    EntityParams
  deriving (Show, Eq)

makeLenses ''EntityParams

instance HasText EntityParams where
  text = Model.MarkdownAst.Params.EntityParams.entity
