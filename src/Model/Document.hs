module Model.Document
  ( Document (..),
  )
where

import qualified Data.Text as T
import Model.MarkdownAst
import qualified Model.Metadata as M
import Path

data Document = Document
  { relPath :: Path Rel File,
    metadata :: M.Metadata,
    ast :: Maybe MarkdownAst,
    text :: T.Text
  }
  deriving (Show, Eq)
