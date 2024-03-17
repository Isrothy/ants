{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Project.DocLoader
  ( loadDocument,
    loadAllFromDirectory,
  )
where

import Commonmark
import Control.Lens
import Data.Default
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Model.Document
import Model.MarkdownAst
import Parser.MarkdownWithFrontmatter
import Path
import Path.IO

loadDocument :: MarkdownSyntax -> Path b Dir -> Path Rel File -> IO Document
loadDocument spec root relPath = do
  let path = root </> relPath
  lastAccessed <- getAccessTime path
  lastModified <- getModificationTime path
  text <- T.pack <$> readFile (toFilePath path)
  let filename = toFilePath (Path.filename path)
  let (mMetadata, ast) = markdownWithFrontmatter spec filename text
  let metadata = fromMaybe (def metadata) mMetadata
  return Document {..}

loadAllFromDirectory :: MarkdownSyntax -> Path b Dir -> IO [Document]
loadAllFromDirectory spec dir = do
  (_, files) <- listDirRecurRel dir
  let mdFiles = filter ((== Just ".md") . fileExtension) files
  mapM (loadDocument spec dir) mdFiles
