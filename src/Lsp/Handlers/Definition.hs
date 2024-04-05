{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Lsp.Handlers.Definition
  ( textDocumentDefinitionHandler,
  )
where

import Control.Applicative
import Control.Lens (use, (.=), (^.))
import Control.Monad.RWS (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Text qualified as T
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Lsp.Handlers.Util
import Lsp.State
import Lsp.Util
import Model.Document (Document (..))
import Model.MarkdownAst
import Model.MarkdownAst.Lenses (block)
import Model.Metadata
import Parser.Markdown
import Parser.MarkdownWithFrontmatter
import Path
import Project.Link

data LinkResult where
  LinkResult ::
    { _path :: Path Abs File,
      _frontMatter :: Metadata,
      _targetText :: T.Text,
      _bookmarkResult :: BookmarkResult
    } ->
    LinkResult

data DefinitionAnalysisResult where
  IsLink ::
    LSP.Range ->
    (Either LinkException LinkResult) ->
    DefinitionAnalysisResult
  IsFootnote ::
    LSP.Range -> DefinitionAnalysisResult

linkAnalysis ::
  MarkdownSyntax ->
  LSP.Uri ->
  LSP.Position ->
  LSP.LspT ServerConfig IO (Maybe DefinitionAnalysisResult)
linkAnalysis spec origUri pos = do
  runMaybeT $ do
    origFile <- MaybeT $ LSP.getVirtualFile (LSP.toNormalizedUri origUri)
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
    (sr, targetFilePath, mbookmark) <- MaybeT $ return $ linkFromAst origAst (l, c)
    rg <- MaybeT $ return $ listToMaybe $ sourceRangeToRange origFile sr
    ret <- lift $ runExceptT $ do
      doc <- ExceptT $ linkedFile spec origPath targetFilePath
      return $
        LinkResult (absPath doc) (metadata doc) (text doc) $
          case mbookmark of
            Nothing -> NoBookmark $ frontMatterLines (text doc) + 1
            Just bookmark ->
              maybe
                BookmarkNotFound
                BookmarkFound
                (ast doc >>= findBookmarkLine bookmark)
    return $ IsLink rg ret

footnoteAnalysis ::
  MarkdownSyntax ->
  LSP.Uri ->
  LSP.Position ->
  LSP.LspT ServerConfig IO (Maybe DefinitionAnalysisResult)
footnoteAnalysis spec origUri pos = do
  runMaybeT $ do
    origFile <- MaybeT $ LSP.getVirtualFile (LSP.toNormalizedUri origUri)
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
    footnoteRef <- MaybeT $ return $ footnoteRefAt l c origAst
    bl <- MaybeT $ return $ listToMaybe $ footnoteRef ^. (parameters . block)
    sr <- MaybeT $ return $ bl ^. sourceRange
    rg <- MaybeT $ return $ listToMaybe $ sourceRangeToRange origFile sr
    return $ IsFootnote rg

definitionAnalysis ::
  MarkdownSyntax ->
  LSP.Uri ->
  LSP.Position ->
  LSP.LspT ServerConfig IO (Maybe DefinitionAnalysisResult)
definitionAnalysis spec origUri pos = do
  link <- linkAnalysis spec origUri pos
  footnote <- footnoteAnalysis spec origUri pos
  return $ link <|> footnote

textDocumentDefinitionHandler :: LSP.Handlers HandlerM
textDocumentDefinitionHandler =
  LSP.requestHandler LSP.SMethod_TextDocumentDefinition \request respond -> do
    handleErrorWithDefault respond (LSP.InR $ LSP.InR LSP.Null) do
      let LSP.TRequestMessage _ _ _ (LSP.DefinitionParams doc pos _ _) = request
          LSP.TextDocumentIdentifier origUri = doc
      spec <- use markdownSyntaxSpec
      ret <- liftLSP $ definitionAnalysis spec origUri pos
      case ret of
        Nothing -> respond $ Right $ LSP.InR $ LSP.InR LSP.Null
        Just (IsLink _ linkResult) -> case linkResult of
          Right (LinkResult path _ _ bookmarkResult) ->
            let uri = LSP.filePathToUri (toFilePath path)
                rg = case bookmarkResult of
                  NoBookmark ln -> LSP.mkRange (fromIntegral ln - 1) 0 (fromIntegral ln) 0
                  BookmarkFound ln -> LSP.mkRange (fromIntegral ln - 1) 0 (fromIntegral ln) 0
                  BookmarkNotFound -> LSP.mkRange 0 0 1 0
             in respond $ Right $ LSP.InL $ LSP.Definition $ LSP.InL $ LSP.Location uri rg
          _ -> respond $ Right $ LSP.InR $ LSP.InR LSP.Null
        Just (IsFootnote rg) -> respond $ Right $ LSP.InL $ LSP.Definition $ LSP.InL $ LSP.Location origUri rg
