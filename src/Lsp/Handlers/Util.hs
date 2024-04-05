{-# LANGUAGE ImportQualifiedPost #-}

module Lsp.Handlers.Util
  ( handleErrorWithDefault,
    linkFromAst,
    linkedFile,
    BookmarkResult (..),
    LinkException (..),
  )
where

import Commonmark
import Control.Conditional
import Control.Lens ((^.))
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Text qualified as T
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Lsp.State
import Lsp.Util
import Model.Document
import Model.MarkdownAst
import Model.MarkdownAst.Lenses (HasTarget (target))
import Parser.Markdown (MarkdownSyntax)
import Path
import Project.DocLoader
import Project.Link

data BookmarkResult = NoBookmark Int | BookmarkNotFound | BookmarkFound Int

data LinkException
  = TargetNotFound
  | OutOfRange
  | FileFormatNotSupported

linkedFile ::
  MarkdownSyntax ->
  Path Abs File ->
  FilePath ->
  LSP.LspT ServerConfig IO (Either LinkException Document)
linkedFile spec origPath linkPath = runExceptT $ do
  targetPath <- maybeToExceptT TargetNotFound $ MaybeT $ liftIO $ resolveLinkInFile origPath linkPath
  root <- maybeToExceptT TargetNotFound $ MaybeT $ fmap (>>= parseAbsDir) LSP.getRootPath
  relPath <- maybeToExceptT OutOfRange $ MaybeT $ return $ stripProperPrefix root targetPath
  when (isHiddenFile relPath) $ throwE OutOfRange
  unless (fileExtension targetPath == Just ".md") $ throwE FileFormatNotSupported
  maybeToExceptT TargetNotFound $ MaybeT $ loadLocalOrVirtualDocument spec root relPath

handleErrorWithDefault ::
  (Either a1 b -> HandlerM a2) ->
  b ->
  HandlerM a2 ->
  HandlerM a2
handleErrorWithDefault respond _default = flip catchE handler
  where
    handler (Log, _message) = do
      liftLSP $
        LSP.sendNotification
          LSP.SMethod_WindowLogMessage
          (LSP.LogMessageParams LSP.MessageType_Log _message)
      respond (Right _default)
    handler (severity_, _message) = do
      let _xtype = case severity_ of
            Error -> LSP.MessageType_Error
            Warning -> LSP.MessageType_Warning
            Info -> LSP.MessageType_Info
      liftLSP $
        LSP.sendNotification
          LSP.SMethod_WindowShowMessage
          (LSP.ShowMessageParams _xtype _message)
      respond (Right _default)

linkFromAst :: (Integral i) => MarkdownAst -> (i, i) -> Maybe (SourceRange, FilePath, Maybe Bookmark)
linkFromAst ast (l, c) = do
  let fromLink :: MdNode -> Maybe T.Text
      fromLink (AstNode (Link ldata) _ _) = Just (ldata ^. target)
      fromLink (AstNode (WikiLink ldata) _ _) = Just (ldata ^. target)
      fromLink _ = Nothing
  ele <- nodeAt isLink l c ast
  link <- fromLink ele
  (filePath, mtag) <- parseLink link
  sr <- ele ^. sourceRange
  return (sr, filePath, mtag)
