{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lsp.Handlers.Reference
  ( textDocumentReferencesHandler,
  )
where

import Control.Lens (use, (^.))
import Control.Monad.Extra (concatMapM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Lsp.Handlers.Util
import Lsp.State
import Lsp.Util
import Model.Document (Document (..))
import Model.MarkdownAst
import Parser.MarkdownWithFrontmatter
import Path
import Project.DocLoader (isHiddenFile, loadAllFromDirectory)
import Project.Link

textDocumentReferencesHandler :: LSP.Handlers HandlerM
textDocumentReferencesHandler =
  LSP.requestHandler LSP.SMethod_TextDocumentReferences \request respond -> do
    handleErrorWithDefault respond (LSP.InR LSP.Null) do
      let LSP.TRequestMessage _ _ _ (LSP.ReferenceParams doc pos _ _ _) = request
          LSP.TextDocumentIdentifier origUri = doc
      spec <- use markdownSyntaxSpec
      items <- runMaybeT $ do
        root <- MaybeT $ liftLSP $ fmap (>>= parseAbsDir) LSP.getRootPath
        origFile <- MaybeT $ liftLSP $ LSP.getVirtualFile (LSP.toNormalizedUri origUri)
        origPath <- MaybeT $ return $ uriToFile origUri
        dataPointPos <- MaybeT $ return $ VFS.positionToCodePointPosition origFile pos
        let l = dataPointPos ^. VFS.line + 1
            c = dataPointPos ^. VFS.character + 1
        origAst <-
          MaybeT $
            return $
              snd $
                markdownWithFrontmatter spec (toFilePath origPath) $
                  VFS.virtualFileText origFile
        header <- MaybeT $ return $ headerAt l c origAst
        id <- MaybeT $ return $ lookup "id" (header ^. attributes)
        docs <- liftIO $ loadAllFromDirectory spec root
        let referenceFromLocalDoc doc = do
              let path = absPath doc
              let uri = LSP.filePathToUri $ toFilePath path
              mvfile <- liftLSP $ LSP.getVirtualFile $ LSP.toNormalizedUri uri
              let mAst = case mvfile of
                    Just vfile ->
                      snd $
                        markdownWithFrontmatter spec (toFilePath path) (VFS.virtualFileText vfile)
                    Nothing -> ast doc
              let toRange sr = head case mvfile of
                    Just vfile -> sourceRangeToRange vfile sr
                    Nothing -> sourceRangeToRangeT (text doc) sr
              case mAst of
                Nothing -> return []
                Just ast ->
                  let links = linksTo root path ast (toFilePath origPath, Just id)
                      refs = fmap (map (LSP.Location uri . toRange)) links
                   in liftLSP $ liftIO refs
        MaybeT $ Just <$> concatMapM referenceFromLocalDoc docs
      respond $ Right $ LSP.InL $ fromMaybe [] items
