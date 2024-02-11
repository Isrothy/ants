{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Project.DocLoader
  ( loadDocumentFromPath,
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
import qualified Path as P
import System.Directory
import Text.Parsec (parse)

loadDocumentFromPath ::
  SyntaxSpec Identity MarkdownAst MarkdownAst ->
  P.Path P.Rel P.File ->
  IO Document
loadDocumentFromPath spec path = do
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

loadAllFromDirectory ::
  SyntaxSpec Identity MarkdownAst MarkdownAst ->
  P.Path b P.Dir ->
  IO Document
loadAllFromDirectory spec dir = do
  docs <- listDirectory (P.toFilePath dir)
  let paths = map (dir </>) docs
  docs <- mapM (loadDocumentFromPath spec) paths
  return (head docs)
