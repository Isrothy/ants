{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Project.DocLoader
  ( loadDocument,
    loadAllFromDirectory,
  )
where

import Data.Default
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Model.Document
import Parser.MarkdownWithFrontmatter
import Path
import Path.IO

loadDocument :: MarkdownSyntax -> Path Abs Dir -> Path Rel File -> IO Document
loadDocument spec root relPath = do
  let absPath = root </> relPath
  lastAccessed <- getAccessTime absPath
  lastModified <- getModificationTime absPath
  text <- T.pack <$> readFile (toFilePath absPath)
  let filename = toFilePath (Path.filename absPath)
  let (mMetadata, ast) = markdownWithFrontmatter spec filename text
  let metadata = fromMaybe (def metadata) mMetadata
  return Document {..}

loadAllFromDirectory :: MarkdownSyntax -> Path Abs Dir -> IO [Document]
loadAllFromDirectory spec dir = do
  (_, files) <- listDirRecurRel dir
  let mdFiles = filter ((/= "default.md") . toFilePath . Path.filename) $ filter ((== Just ".md") . fileExtension) files
  mapM (loadDocument spec dir) mdFiles
