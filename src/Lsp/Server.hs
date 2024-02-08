{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lsp.Server (handlers) where

import Control.Lens (to, (^.))
import Control.Monad.IO.Class
import Data.Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as Text
import Language.LSP.Protocol.Lens qualified as J
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Types qualified as J
import Language.LSP.Server
import System.IO (stderr)

debug :: (MonadIO m) => T.Text -> m ()
debug msg = liftIO $ Text.hPutStrLn stderr $ "[ants-lsp] " <> msg

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        debug "Initialized",
      requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
            Position _l _c' = pos
            rsp = Hover (InL ms) (Just range)
            ms = mkMarkdown "Hello world"
            range = Range pos pos
        responder (Right $ InL rsp),
      requestHandler SMethod_TextDocumentDefinition $ \req responder -> do
        let TRequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier doc) pos _ _) = req
        responder (Right $ InL $ Definition $ InL $ Location doc $ Range pos pos)
    ]
