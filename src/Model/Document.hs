module Model.Document
  ( Document (..),
  )
where

import Model.MarkdownAst
import qualified Model.Metadata as M
import Path

data Document = Document
  { relPath :: Path Rel File,
    metadata :: M.Metadata,
    ast :: Maybe MarkdownAst
  }
  deriving (Show, Eq)
