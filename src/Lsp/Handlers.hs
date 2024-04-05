{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lsp.Handlers
  ( handlers,
  )
where

import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Server qualified as LSP
import Lsp.Handlers.Completion
import Lsp.Handlers.Definition
import Lsp.Handlers.Hover
import Lsp.Handlers.Initialized
import Lsp.Handlers.Reference
import Lsp.State

workspaceChangeConfigurationHandler :: LSP.Handlers HandlerM
workspaceChangeConfigurationHandler =
  LSP.notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration \_ -> return ()

textDocumentChangeHandler :: LSP.Handlers HandlerM
textDocumentChangeHandler =
  LSP.notificationHandler LSP.SMethod_TextDocumentDidChange \_ -> return ()

cancelationHandler :: LSP.Handlers HandlerM
cancelationHandler =
  LSP.notificationHandler LSP.SMethod_CancelRequest \_ -> return ()

didOpenTextDocumentNotificationHandler :: LSP.Handlers HandlerM
didOpenTextDocumentNotificationHandler =
  LSP.notificationHandler LSP.SMethod_TextDocumentDidOpen \_ -> return ()

didSaveTextDocumentNotificationHandler :: LSP.Handlers HandlerM
didSaveTextDocumentNotificationHandler =
  LSP.notificationHandler LSP.SMethod_TextDocumentDidSave \_ -> return ()

didCloseTextDocumentNotificationHandler :: LSP.Handlers HandlerM
didCloseTextDocumentNotificationHandler =
  LSP.notificationHandler LSP.SMethod_TextDocumentDidClose \_ -> return ()

handlers :: LSP.Handlers HandlerM
handlers =
  mconcat
    [ initializedHandler,
      textDocumentHoverHandler,
      textDocumentDefinitionHandler,
      textDocumentReferencesHandler,
      workspaceChangeConfigurationHandler,
      textDocumentChangeHandler,
      cancelationHandler,
      didOpenTextDocumentNotificationHandler,
      didSaveTextDocumentNotificationHandler,
      didCloseTextDocumentNotificationHandler,
      completionHandler
    ]
