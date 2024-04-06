{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Lsp.Handlers.Hover
  ( textDocumentHoverHandler,
  )
where

import Control.Applicative ((<|>))
import Control.Lens (use, (.=), (^.))
import Control.Monad.RWS (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as B
import Data.Text.LineBreaker qualified as T
import Data.Yaml.Pretty (defConfig, encodePretty)
import Language.LSP.Protocol.Lens (HasLine (line), HasStart (start))
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Lsp.Handlers.Util
import Lsp.State
import Lsp.Util
import Model.Document (Document (..))
import Model.MarkdownAst
import Model.MarkdownAst.Lenses (HasBlock (..))
import Model.Metadata
import Parser.Markdown
import Parser.MarkdownWithFrontmatter
import Path
import Project.Link

formatLinkException :: LinkException -> T.Text
formatLinkException TargetNotFound = "**ERROR**: Target Not Found"
formatLinkException OutOfRange = "**WARNING**: Out Of Range"
formatLinkException FileFormatNotSupported = "**INFO**: File Format Not Supported"

data LinkResult where
  LinkResult ::
    { _path :: Path Abs File,
      _frontMatter :: Metadata,
      _targetText :: T.Text,
      _bookmarkResult :: BookmarkResult
    } ->
    LinkResult

data FootnoteResult where
  FootnoteResult ::
    { _range :: LSP.Range,
      _footnoteText :: T.Text
    } ->
    FootnoteResult

data HoverAnalysisResult where
  IsLink ::
    LSP.Range ->
    (Either LinkException LinkResult) ->
    HoverAnalysisResult
  IsFootnote ::
    LSP.Range -> FootnoteResult -> HoverAnalysisResult

formatLinkResult :: LinkResult -> T.Text
formatLinkResult (LinkResult path frontMatter targetText bookmarkResult) =
  TL.toStrict . B.toLazyText $ displayFilepath <> displayFrontMatter <> displayContent
  where
    lineCount = 10
    displayFilepath = "In `" <> (B.fromText . T.pack . toFilePath) path <> "` :\n\n"
    displayFrontMatter =
      "```yaml\n"
        <> B.fromText (TE.decodeUtf8With TEE.lenientDecode (encodePretty defConfig frontMatter))
        <> "```\n\n"
    displayContent = case bookmarkResult of
      NoBookmark ln ->
        B.fromText
          (T.joinLines $ take lineCount $ drop (ln - 1) $ T.splitLines targetText)
      BookmarkNotFound -> "**ERROR**: Bookmark Not Found \n"
      BookmarkFound ln ->
        "Line "
          <> (B.fromText . T.pack . show) ln
          <> ":\n\n"
          <> B.fromText (T.joinLines $ take lineCount $ drop (ln - 1) $ T.splitLines targetText)

formatFootnoteResult :: FootnoteResult -> T.Text
formatFootnoteResult (FootnoteResult rg t) = "From line " <> T.pack (show (fromIntegral $ rg ^. (start . line))) <> ":\n" <> t

linkAnalysis ::
  MarkdownSyntax ->
  LSP.Uri ->
  LSP.Position ->
  LSP.LspT ServerConfig IO (Maybe HoverAnalysisResult)
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
  LSP.LspT ServerConfig IO (Maybe HoverAnalysisResult)
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
    let bl = footnoteRef ^. (parameters . block)
    let findSr = firstNode' (^. sourceRange)
    targetSr <- MaybeT $ return $ findSr bl
    sr <- MaybeT $ return $ footnoteRef ^. sourceRange
    rg <- MaybeT $ return $ listToMaybe $ sourceRangeToRange origFile sr
    targetRg <- MaybeT $ return $ listToMaybe $ sourceRangeToRange origFile targetSr
    let ln = fromIntegral $ targetRg ^. (start . line)
    let origText = VFS.virtualFileText origFile
    return $ IsFootnote rg $ FootnoteResult targetRg $ T.joinLines $ take 5 $ drop ln $ T.splitLines origText

hoverAnalysis ::
  MarkdownSyntax ->
  LSP.Uri ->
  LSP.Position ->
  LSP.LspT ServerConfig IO (Maybe HoverAnalysisResult)
hoverAnalysis spec origUri pos = do
  link <- linkAnalysis spec origUri pos
  footnote <- footnoteAnalysis spec origUri pos
  return $ link <|> footnote

textDocumentHoverHandler :: LSP.Handlers HandlerM
textDocumentHoverHandler =
  LSP.requestHandler LSP.SMethod_TextDocumentHover \request respond ->
    handleErrorWithDefault respond (LSP.InR LSP.Null) do
      let LSP.TRequestMessage _ _ _ (LSP.HoverParams doc pos _) = request
          LSP.TextDocumentIdentifier origUri = doc
      spec <- use markdownSyntaxSpec
      ret <- liftLSP $ hoverAnalysis spec origUri pos
      case ret of
        Nothing -> respond $ Right $ LSP.InR LSP.Null
        Just (IsLink rg linkResult) ->
          respond $
            Right $
              LSP.InL $
                LSP.Hover
                  (LSP.InL $ LSP.mkMarkdown $ either formatLinkException formatLinkResult linkResult)
                  (Just rg)
        Just (IsFootnote rg footnoteResult) ->
          respond $ Right $ LSP.InL $ LSP.Hover (LSP.InL $ LSP.mkPlainText $ formatFootnoteResult footnoteResult) (Just rg)
