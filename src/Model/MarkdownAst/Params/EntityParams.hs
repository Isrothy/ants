{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.EntityParams where

import Control.Lens
import Data.Text qualified as T

data EntityParams where
  EntityParams ::
    { _entity :: T.Text
    } ->
    EntityParams
  deriving (Show, Eq)

makeLenses ''EntityParams
