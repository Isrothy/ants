{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lsp.Util
  ( uriToDir,
    uriToFile,
    readUri,
    liftLSP,
    sourceRangeToRange,
  )
where

import Commonmark
import Control.Monad.RWS
import Control.Monad.Trans.Except qualified as Except
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.Utf16.Rope qualified as Rope
import Data.Yaml.Pretty
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as LSP.Types
import Language.LSP.Server
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS
import Language.LSP.VFS qualified as LSP
import Lsp.State
import Path
import Text.RawString.QQ

uriToDir :: Uri -> Maybe (Path Abs Dir)
uriToDir uriStr = uriToFilePath uriStr >>= parseAbsDir

uriToFile :: Uri -> Maybe (Path Abs File)
uriToFile uriStr = uriToFilePath uriStr >>= parseAbsFile

liftLSP :: LspT ServerConfig IO a -> HandlerM a
liftLSP m = lift (lift m)

readUri :: Uri -> HandlerM Text.Text
readUri uri_ = do
  mVirtualFile <- liftLSP (LSP.getVirtualFile (LSP.Types.toNormalizedUri uri_))
  case mVirtualFile of
    Just (LSP.VirtualFile _ _ rope) -> return (Rope.toText rope)
    Nothing -> Except.throwE (Error, "Could not find " <> Text.pack (show uri_) <> " in VFS.")

sourceRangeToRange :: SourceRange -> [Range]
sourceRangeToRange sr = map helper (unSourceRange sr)
  where
    helper (begin, end) =
      Range
        ( Position
            (fromIntegral $ sourceLine begin - 1)
            (fromIntegral $ sourceColumn begin - 1)
        )
        ( Position
            (fromIntegral $ sourceLine end - 1)
            (fromIntegral $ sourceColumn end - 1)
        )
