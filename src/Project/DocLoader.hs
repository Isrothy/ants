{-# OPTIONS_GHC -Wno-name-shadowing #-}

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
import qualified Model.Document as D
import Model.MarkdownAst
import Parser.MarkdownWithFrontmatter
import Path
import Path.IO
import Text.Parsec (parse)

loadDocument ::
  SyntaxSpec Identity MarkdownAst MarkdownAst ->
  Path b Dir ->
  Path Rel File ->
  IO D.Document
loadDocument spec anker relPath = do
  let path = anker </> relPath
  timeCreated <- getModificationTime path
  lastModified <- getModificationTime path
  text <- T.pack <$> readFile (toFilePath path)
  let fn = toFilePath (filename path)
  let result = parse (markdownWithFrontmatter spec fn) fn text
  let (metadata, ast) = fromRight (Nothing, Nothing) result
  return
    D.Document
      { D.relPath = relPath,
        D.timeCreated = timeCreated,
        D.lastModified = lastModified,
        D.filename = fn,
        D.metadata = def metadata,
        D.ast = ast,
        D.text = text
      }

loadAllFromDirectory ::
  SyntaxSpec Identity MarkdownAst MarkdownAst ->
  Path b Dir ->
  IO [D.Document]
loadAllFromDirectory spec dir = do
  (_, files) <- listDirRecurRel dir
  let mdFiles = filter ((== Just ".md") . fileExtension) files
  mapM (loadDocument spec dir) mdFiles
