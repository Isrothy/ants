{-# LANGUAGE ImportQualifiedPost #-}

module Lsp.Util
  ( uriToDir,
    uriToFile,
    liftLSP,
    sourceRangeToRange,
    readFileSafe,
  )
where

import Commonmark
import Control.Exception
import Control.Monad.RWS
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Lsp.State
import Path

uriToDir :: LSP.Uri -> Maybe (Path Abs Dir)
uriToDir uriStr = LSP.uriToFilePath uriStr >>= parseAbsDir

uriToFile :: LSP.Uri -> Maybe (Path Abs File)
uriToFile uriStr = LSP.uriToFilePath uriStr >>= parseAbsFile

liftLSP :: LSP.LspT ServerConfig IO a -> HandlerM a
liftLSP m = lift (lift m)

readFileSafe :: FilePath -> IO (Maybe T.Text)
readFileSafe filePath = do
  result <- try (TIO.readFile filePath) :: IO (Either IOException T.Text)
  case result of
    Left _ -> return Nothing
    Right content -> return (Just content)

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
