{-# LANGUAGE ImportQualifiedPost #-}

module Lsp.Util
  ( uriToDir,
    uriToFile,
    liftLSP,
    readLocalOrVFS,
    sourceRangeToRange,
    sourcePosToPosition,
    sourceRangeToCodePointRange,
    sourcePosToCodePointPosition,
  )
where

import Commonmark
import Control.Monad.RWS
import Data.Maybe
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Lsp.State
import Path
import Text.Parsec.Pos
import Util.IO (readFileSafe)

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
