{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.MarkdownAst.Params.TaskListParams
  ( TaskListParams (..),
  )
where

import Commonmark (ListType)
import Commonmark.Types (ListSpacing)
import Control.Lens
import Model.MarkdownAst.Classes.HasListSpacing
import Model.MarkdownAst.Classes.HasListType
import Model.MarkdownAst.Classes.HasTaskListItems

data TaskListParams bl where
  TaskListParams ::
    { _listType :: ListType,
      _listSpacing :: ListSpacing,
      _taskListItems :: [(Bool, bl)]
    } ->
    TaskListParams bl

makeLenses ''TaskListParams

instance HasListSpacing (TaskListParams bl) where
  listSpacing = Model.MarkdownAst.Params.TaskListParams.listSpacing

instance HasListType (TaskListParams bl) where
  listType = Model.MarkdownAst.Params.TaskListParams.listType

instance HasTaskListItems TaskListParams bl where
  taskListItems = Model.MarkdownAst.Params.TaskListParams.taskListItems
