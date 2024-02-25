{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}

module Project.DocLoader
  ( loadDocument,
    loadAllFromDirectory,
  )
where

import Commonmark
import Control.Monad.Identity (Identity)
import Data.Default
import Data.Either
import qualified Data.Text as T
import Model.Document
import Model.MarkdownAst
import Parser.MarkdownWithFrontmatter
import Path
import Path.IO
import Text.Parsec (parse)
import Data.Maybe (fromMaybe)

loadDocument ::
  SyntaxSpec Identity MarkdownAst MarkdownAst ->
  Path b Dir ->
  Path Rel File ->
  IO Document
loadDocument spec anker relPath = do
  let path = anker </> relPath
  timeCreated <- getModificationTime path
  lastModified <- getModificationTime path
  text <- T.pack <$> readFile (toFilePath path)
  let filename = toFilePath (Path.filename path)
  let result = markdownWithFrontmatter spec filename text
  let (meta, ast) = result
  let metadata = fromMaybe (def metadata) meta
  return
    Document
      {
        ..
      }

loadAllFromDirectory ::
  SyntaxSpec Identity MarkdownAst MarkdownAst ->
  Path b Dir ->
  IO [Document]
loadAllFromDirectory spec dir = do
  (_, files) <- listDirRecurRel dir
  let mdFiles = filter ((== Just ".md") . fileExtension) files
  mapM (loadDocument spec dir) mdFiles
