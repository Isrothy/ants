module Lsp.Util
  ( uriToDir,
    uriToFile,
  )
where

import Control.Conditional
import Control.Monad.IO.Class
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Lsp.State
import Network.URI (parseURI, uriPath, uriScheme)
import Path
import System.FilePath (isAbsolute)

uriToDir :: Uri -> Maybe (Path Abs Dir)
uriToDir uriStr = uriToFilePath uriStr >>= parseAbsDir

uriToFile :: Uri -> Maybe (Path Abs File)
uriToFile uriStr = uriToFilePath uriStr >>= parseAbsFile
