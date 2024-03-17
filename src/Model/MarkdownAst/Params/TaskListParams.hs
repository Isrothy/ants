{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.TaskListParams
  ( TaskListParams (..),
  )
where

import Commonmark.Types
import Control.Lens
import Model.MarkdownAst.Lenses.HasListSpacing
import Model.MarkdownAst.Lenses.HasListType
import Model.MarkdownAst.Lenses.HasTaskListItems

data TaskListParams il bl where
  TaskListParams ::
    { _listType :: ListType,
      _listSpacing :: ListSpacing,
      _taskListItems :: [(Bool, bl)]
    } ->
    TaskListParams il bl
  deriving (Show, Eq)

makeLenses ''TaskListParams

instance HasListSpacing (TaskListParams il bl) where
  listSpacing = Model.MarkdownAst.Params.TaskListParams.listSpacing

instance HasListType (TaskListParams il bl) where
  listType = Model.MarkdownAst.Params.TaskListParams.listType

instance (IsInline il, IsBlock il bl) => HasTaskListItems TaskListParams il bl where
  taskListItems = Model.MarkdownAst.Params.TaskListParams.taskListItems
