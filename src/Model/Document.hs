{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Model.Document
  ( Document (..),
    fromRelPath,
  )
where

import Commonmark
import Control.Monad.Identity (Identity)
import Data.Default
import Data.Either
import qualified Data.Text as T
import Data.Time
import Model.MarkdownAst
import qualified Model.Metadata as M
import Parser.MarkdownWithFrontmatter
import qualified Path as P
import System.Directory
import Text.Parsec (parse)

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

fromRelPath ::
  SyntaxSpec Identity MarkdownAst MarkdownAst ->
  P.Path P.Rel P.File ->
  IO Document
fromRelPath spec path = do
  timeCreated <- getModificationTime (P.toFilePath path)
  lastModified <- getModificationTime (P.toFilePath path)
  text <- T.pack <$> readFile (P.toFilePath path)
  let filename = P.toFilePath (P.filename path)
  let result = parse (markdownWithFrontmatter spec filename) filename text
  let (metadata, ast) = fromRight (Nothing, Nothing) result
  return
    Document
      { relPath = path,
        timeCreated = timeCreated,
        lastModified = lastModified,
        filename = filename,
        metadata = def metadata,
        ast = ast,
        text = text
      }
