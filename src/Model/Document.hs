module Model.Document
  ( Document (..),
  )
where

import qualified Data.Text as T
import Data.Time
import Model.MarkdownAst
import qualified Model.Metadata as M
import qualified Path as P

data Document = Document
  { relPath :: P.Path P.Rel P.File,
    timeCreated :: !UTCTime,
    lastModified :: !UTCTime,
    filename :: String,
    metadata :: M.Metadata,
    ast :: Maybe MarkdownAst,
    text :: T.Text
  }
  deriving (Show, Eq)
