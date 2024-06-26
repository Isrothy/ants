{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Project.DocLoader
  ( loadDocument,
    loadAllFromDirectory,
    isHiddenFile,
  )
where

import Data.Default
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Model.Document (Document (..))
import Parser.MarkdownWithFrontmatter
import Path
import Path.IO

loadDocument :: MarkdownSyntax -> Path Abs Dir -> Path Rel File -> IO Document
loadDocument spec root relPath = do
  let absPath = root </> relPath
  lastAccessed <- Just <$> getAccessTime absPath
  lastModified <- Just <$> getModificationTime absPath
  text <- T.pack <$> readFile (toFilePath absPath)
  let filename = toFilePath (Path.filename absPath)
  let (mMetadata, ast) = markdownWithFrontmatter spec filename text
  let metadata = fromMaybe def mMetadata
  return Document {..}

isHiddenDir :: Path b Dir -> Bool
isHiddenDir dir
  | parent dir == dir = False
  | otherwise = ("." `isPrefixOf` toFilePath (dirname dir)) || isHiddenDir (parent dir)

isHiddenFile :: Path b File -> Bool
isHiddenFile file = "." `isPrefixOf` toFilePath (Path.filename file) || isHiddenDir (parent file)

loadAllFromDirectory :: MarkdownSyntax -> Path Abs Dir -> IO [Document]
loadAllFromDirectory spec dir = do
  (_, files) <- listDirRecurRel dir
  let mdFiles = filter (not . isHiddenFile) $ filter ((== Just ".md") . fileExtension) files
  mapM (loadDocument spec dir) mdFiles
