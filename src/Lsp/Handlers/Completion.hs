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

module Lsp.Handlers.Completion
  ( completionHandler,
  )
where

import Commonmark (SourceRange (..))
import Commonmark.Types (sourceLine)
import Control.Conditional (guard)
import Control.Lens (use, (^.))
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.LineBreaker qualified as T
import Data.Yaml.Pretty (defConfig, encodePretty)
import Fmt
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Server qualified as LSP
import Language.LSP.VFS qualified as VFS
import Lsp.Handlers.Util
import Lsp.State
import Lsp.Util
import Model.Document (Document (..))
import Model.MarkdownAst
import Model.MarkdownAst.Params.HeaderParams
import Parser.MarkdownWithFrontmatter
import Path
import Safe (atMay)

-- | The LSP handler for 'textDocument/completion'.
-- Currently only supports bookmarks
-- TODO: Add support for footnotes and path
completionHandler :: LSP.Handlers HandlerM
completionHandler =
  LSP.requestHandler LSP.SMethod_TextDocumentCompletion \request respond -> do
    handleErrorWithDefault respond (LSP.InR $ LSP.InL (LSP.CompletionList True Nothing [])) do
      let LSP.TRequestMessage _ _ _ (LSP.CompletionParams doc pos _ _ _) = request
          LSP.TextDocumentIdentifier origUri = doc
      spec <- use markdownSyntaxSpec
      items <- runMaybeT $ do
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
        (_, filepath, bookmark) <- MaybeT $ return $ linkFromAst origAst (l, c)
        guard $ bookmark == Just ""
        let path = fromJust (uriToFile origUri)
        doc <- exceptToMaybeT $ ExceptT $ liftLSP $ linkedFile spec path filepath
        targetAst <- MaybeT $ return $ ast doc
        let lines = T.splitLines $ text doc
        let formatDetail :: Int -> T.Text -> T.Text
            formatDetail lineNr line = fmt ("On line " +| lineNr |+ ": " +| line |+ "")
        let formatDocumentation :: Int -> T.Text
            formatDocumentation lineNr =
              let lineCount = 10
                  displayPath = fmtLn ("In file `" +| toFilePath (absPath doc) |+ "'\n\n")
                  displayFrontMatter = fmtLn ("```yaml\n" +| TE.decodeUtf8With TEE.lenientDecode (encodePretty defConfig (metadata doc)) |+ "\n```\n")
                  displayContent = fmtLn ("Line: " +| lineNr |+ "\n\n" +| T.joinLines (take lineCount $ drop (lineNr - 1) lines) |+ "")
               in displayPath <> displayFrontMatter <> displayContent
        let headerToItem :: MdNode -> Maybe LSP.CompletionItem
            headerToItem (AstNode (Header (HeaderParams _ _)) msr attrs) = do
              label <- lookup "id" attrs
              sr <- msr
              lineNr <- case unSourceRange sr of
                (begin, _) : _ -> Just $ sourceLine begin
                _ -> Nothing
              line <- fmap fst (atMay lines (lineNr - 1))
              return
                LSP.CompletionItem
                  { _label = label,
                    _labelDetails = Nothing,
                    _kind = Just LSP.CompletionItemKind_Reference,
                    _tags = Nothing,
                    _detail = Just $ formatDetail lineNr line,
                    _documentation = Just $ LSP.InR $ LSP.mkMarkdown $ formatDocumentation lineNr,
                    _deprecated = Nothing,
                    _preselect = Nothing,
                    _sortText = Nothing,
                    _filterText = Nothing,
                    _insertText = Nothing,
                    _insertTextFormat = Nothing,
                    _insertTextMode = Nothing,
                    _textEdit = Nothing,
                    _textEditText = Nothing,
                    _additionalTextEdits = Nothing,
                    _commitCharacters = Nothing,
                    _command = Nothing,
                    _data_ = Nothing
                  }
            headerToItem _ = Nothing
        return $ allNodes' headerToItem targetAst
      respond $ Right $ LSP.InR $ LSP.InL $ LSP.CompletionList False Nothing $ fromMaybe [] items
