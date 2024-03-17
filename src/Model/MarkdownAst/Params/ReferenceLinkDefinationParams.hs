{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.ReferenceLinkDefinationParams
  ( ReferenceLinkDefinationParams (..),
  )
where

import Control.Lens
import Data.Text qualified as T
import Model.MarkdownAst.Lenses.HasLabel
import Model.MarkdownAst.Lenses.HasTarget
import Model.MarkdownAst.Lenses.HasTitle

data ReferenceLinkDefinationParams where
  ReferenceLinkDefinationData ::
    { _label :: T.Text,
      _target :: T.Text,
      _title :: T.Text
    } ->
    ReferenceLinkDefinationParams
  deriving (Show, Eq)

makeLenses ''ReferenceLinkDefinationParams

instance HasTitle ReferenceLinkDefinationParams where
  title = Model.MarkdownAst.Params.ReferenceLinkDefinationParams.title

instance HasTarget ReferenceLinkDefinationParams where
  target = Model.MarkdownAst.Params.ReferenceLinkDefinationParams.target

instance HasLabel ReferenceLinkDefinationParams where
  label = Model.MarkdownAst.Params.ReferenceLinkDefinationParams.label
