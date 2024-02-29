{-# LANGUAGE ImportQualifiedPost #-}

module Lsp.Util
  ( uriToDir,
    uriToFile,
    liftLSP,
    sourceRangeToRange,
    readLocalOrVFS,
  )
where

import Commonmark
import Control.Monad.RWS
import Data.Text qualified as T
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Lsp.State
import Path
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

sourceRangeToRange :: SourceRange -> [LSP.Range]
sourceRangeToRange sr = map helper (unSourceRange sr)
  where
    helper (begin, end) =
      LSP.Range
        ( LSP.Position
            (fromIntegral $ sourceLine begin - 1)
            (fromIntegral $ sourceColumn begin - 1)
        )
        ( LSP.Position
            (fromIntegral $ sourceLine end - 1)
            (fromIntegral $ sourceColumn end - 1)
        )
