{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module Lsp.Util
  ( uriToDir,
    uriToFile,
    liftLSP,
    readLocalOrVFS,
    sourceRangeToRange,
    sourcePosToPosition,
    sourceRangeToCodePointRange,
    sourcePosToCodePointPosition,
    loadLocalOrVirtualDocument,
  )
where

import Commonmark
import Control.Monad.RWS
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Maybe
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Lsp.State
import Model.Document
import Model.MarkdownAst
import Model.Metadata
import Parser.Markdown
import Parser.MarkdownWithFrontmatter (markdownWithFrontmatter)
import Path
import Project.DocLoader
import Text.Parsec.Pos
import Util.IO (readFileSafe, safeIO)

uriToDir :: LSP.Uri -> Maybe (Path Abs Dir)
uriToDir uriStr = LSP.uriToFilePath uriStr >>= parseAbsDir

uriToFile :: LSP.Uri -> Maybe (Path Abs File)
uriToFile uriStr = LSP.uriToFilePath uriStr >>= parseAbsFile

liftLSP :: LSP.LspT ServerConfig IO a -> HandlerM a
liftLSP m = lift (lift m)

readLocalOrVFS :: Path Abs File -> LSP.LspT ServerConfig IO (Maybe T.Text)
readLocalOrVFS path = do
  mfile <- LSP.getVirtualFile (LSP.toNormalizedUri (LSP.filePathToUri (toFilePath path)))
  case mfile of
    Nothing -> liftIO $ readFileSafe (toFilePath path)
    Just vf -> return $ Just $ VFS.virtualFileText vf

sourcePosToCodePointPosition :: SourcePos -> VFS.CodePointPosition
sourcePosToCodePointPosition sp =
  VFS.CodePointPosition
    (fromIntegral $ sourceLine sp - 1)
    (fromIntegral $ sourceColumn sp - 1)

sourcePosToPosition :: VFS.VirtualFile -> SourcePos -> Maybe LSP.Position
sourcePosToPosition vf sp = VFS.codePointPositionToPosition vf (sourcePosToCodePointPosition sp)

sourceRangeToCodePointRange :: SourceRange -> [VFS.CodePointRange]
sourceRangeToCodePointRange sr = map helper (unSourceRange sr)
  where
    helper (begin, end) =
      VFS.CodePointRange
        (sourcePosToCodePointPosition begin)
        (sourcePosToCodePointPosition end)

sourceRangeToRange :: VFS.VirtualFile -> SourceRange -> [LSP.Range]
sourceRangeToRange vf sr = mapMaybe (VFS.codePointRangeToRange vf) (sourceRangeToCodePointRange sr)

loadLocalOrVirtualDocument ::
  MarkdownSyntax ->
  Path Abs Dir ->
  Path Rel File ->
  LSP.LspT ServerConfig IO (Maybe Document)
loadLocalOrVirtualDocument spec root relPath = do
  let absPath = root </> relPath
  let uri = LSP.filePathToUri $ toFilePath $ root </> relPath
  mfile <- LSP.getVirtualFile $ LSP.toNormalizedUri uri
  case mfile of
    Nothing -> liftIO $ safeIO $ loadDocument spec root relPath
    Just virtualFile -> return $ do
      let lastAccessed = Nothing
      let lastModified = Nothing
      let text = VFS.virtualFileText virtualFile
      let (mMetadata, ast) = markdownWithFrontmatter spec (toFilePath absPath) text
      let filename = toFilePath (Path.filename absPath)
      let metadata = fromMaybe def mMetadata
      return $ Document {..}
