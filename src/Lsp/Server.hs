{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Lsp.Server
  ( handlers,
    textDocumentHoverHandler,
    initializedHandler,
    textDocumentDefinitionHandler,
  )
where

import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.IO qualified as Text
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import System.IO (stderr)

debug :: (MonadIO m) => T.Text -> m ()
debug msg = liftIO $ Text.hPutStrLn stderr $ "[ants-lsp] " <> msg

textDocumentHoverHandler ::
  forall {f :: MessageDirection} {m :: Method f Request} {a} {b} {t}.
  (MessageParams m ~ HoverParams) =>
  TRequestMessage m ->
  (Either a (Hover |? b) -> t) ->
  t
textDocumentHoverHandler req responder = do
  let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
      Position _l _c' = pos
      rsp = Hover (InL ms) (Just range)
      ms = mkMarkdown "Hello world"
      range = Range pos pos
  responder (Right $ InL rsp)

initializedHandler :: (MonadIO m) => p -> m ()
initializedHandler _not = do
  debug "Initialized"

textDocumentDefinitionHandler ::
  forall {f :: MessageDirection} {m :: Method f Request} {a} {b} {t}.
  (MessageParams m ~ DefinitionParams) =>
  TRequestMessage m ->
  (Either a (Definition |? b) -> t) ->
  t
textDocumentDefinitionHandler req responder = do
  let TRequestMessage _ _ _ (DefinitionParams (TextDocumentIdentifier doc) pos _ _) = req
  responder (Right $ InL $ Definition $ InL $ Location doc $ Range pos pos)

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized initializedHandler,
      requestHandler SMethod_TextDocumentHover textDocumentHoverHandler,
      requestHandler SMethod_TextDocumentDefinition textDocumentDefinitionHandler
    ]
