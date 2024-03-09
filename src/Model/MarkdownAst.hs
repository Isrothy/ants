{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Model.MarkdownAst
  ( MarkdownAst,
    MarkdownElement (..),
    MarkdownAstNode (..),
    findPlaceholders,
    findLinks,
    findTasks,
    findFinishedTasks,
    findUnfinishedTasks,
    findAlerts,
    firstNode,
    allNodes,
    nodeAt,
    firstNode',
    allNodes',
    nodeAt',
    LinkData (..),
    ImageData (..),
    ReferenceLinkDefinationData (..),
    attributes,
    element,
    sourceRange,
    linkTarget,
    linkTitle,
    linkInline,
    imageSource,
    imageTitle,
    imageInline,
    listType,
    listSpacing,
    listItems,
    referenceLinkDefinationLabel,
    referenceLinkDefinationDestination,
    referenceLinkDefinationTitle,
    wikiLinkTarget,
    wikiLinkInline,
  )
where

import Commonmark
import Commonmark.Extensions
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Internal.Builder as TB
import qualified Data.Text.Lazy as LT
import Parser.Placeholder

data MarkdownAstNode where
  MarkdownAstNode ::
    { _element :: MarkdownElement,
      _sourceRange :: Maybe SourceRange,
      _attributes :: Attributes
    } ->
    MarkdownAstNode
  deriving (Show, Eq)

type MarkdownAst = [MarkdownAstNode]

data MarkdownElement where
  Text :: T.Text -> MarkdownElement
  Entity :: T.Text -> MarkdownElement
  LineBreak :: MarkdownElement
  SoftBreak :: MarkdownElement
  EscapedChar :: Char -> MarkdownElement
  Code :: T.Text -> MarkdownElement
  Emphasis :: MarkdownAst -> MarkdownElement
  Strong :: MarkdownAst -> MarkdownElement
  Link :: LinkData -> MarkdownElement
  Image :: ImageData -> MarkdownElement
  Strikethrough :: MarkdownAst -> MarkdownElement
  Highlight :: MarkdownAst -> MarkdownElement
  RawInline :: Format -> T.Text -> MarkdownElement
  Alert :: AlertType -> MarkdownAst -> MarkdownElement
  Emoji :: T.Text -> T.Text -> MarkdownElement
  InlineMath :: T.Text -> MarkdownElement
  DisplayMath :: T.Text -> MarkdownElement
  SingleQuoted :: MarkdownAst -> MarkdownElement
  DoubleQuoted :: MarkdownAst -> MarkdownElement
  Subscript :: MarkdownAst -> MarkdownElement
  Superscript :: MarkdownAst -> MarkdownElement
  Paragraph :: MarkdownAst -> MarkdownElement
  Plain :: MarkdownAst -> MarkdownElement
  Header :: Int -> MarkdownAst -> MarkdownElement
  List :: ListData -> MarkdownElement
  Blockquote :: MarkdownAst -> MarkdownElement
  CodeBlock :: T.Text -> T.Text -> MarkdownElement
  RawBlock :: Format -> T.Text -> MarkdownElement
  HorizontalRule :: MarkdownElement
  PipeTable ::
    [ColAlignment] ->
    [MarkdownAst] ->
    [[MarkdownAst]] ->
    MarkdownElement
  ReferenceLinkDefination :: ReferenceLinkDefinationData -> MarkdownElement
  DefinitionList ::
    ListSpacing ->
    [(MarkdownAst, [MarkdownAst])] ->
    MarkdownElement
  TaskList ::
    ListType ->
    ListSpacing ->
    [(Bool, MarkdownAst)] ->
    MarkdownElement
  WikiLink :: WikiLinkData -> MarkdownElement
  Footnote :: Int -> T.Text -> MarkdownAst -> MarkdownElement
  FootnoteList :: [MarkdownAst] -> MarkdownElement
  FootnoteRef :: T.Text -> T.Text -> MarkdownAst -> MarkdownElement
  Placeholder :: T.Text -> MarkdownElement
  Span :: MarkdownAst -> MarkdownElement
  deriving (Show, Eq)

data LinkData where
  LinkData ::
    { _linkTarget :: T.Text,
      _linkTitle :: T.Text,
      _linkInline :: MarkdownAst
    } ->
    LinkData
  deriving (Show, Eq)

data ImageData where
  ImageData ::
    { _imageSource :: T.Text,
      _imageTitle :: T.Text,
      _imageInline :: MarkdownAst
    } ->
    ImageData
  deriving (Show, Eq)

data WikiLinkData where
  WikiLinkData ::
    { _wikiLinkTarget :: T.Text,
      _wikiLinkInline :: MarkdownAst
    } ->
    WikiLinkData
  deriving (Show, Eq)

data ListData where
  ListData ::
    { _listType :: ListType,
      _listSpacing :: ListSpacing,
      _listItems :: [MarkdownAst]
    } ->
    ListData
  deriving (Show, Eq)

data ReferenceLinkDefinationData where
  ReferenceLinkDefinationData ::
    { _referenceLinkDefinationLabel :: T.Text,
      _referenceLinkDefinationDestination :: T.Text,
      _referenceLinkDefinationTitle :: T.Text
    } ->
    ReferenceLinkDefinationData
  deriving (Show, Eq)

makeLenses ''MarkdownAstNode

makeLenses ''LinkData

makeLenses ''WikiLinkData

makeLenses ''ImageData

makeLenses ''ListData

makeLenses ''ReferenceLinkDefinationData

rawNode :: MarkdownElement -> MarkdownAst
rawNode x = [MarkdownAstNode x Nothing []]

instance HasAttributes MarkdownAst where
  addAttributes _ [] = []
  addAttributes attrs1 [MarkdownAstNode item sr attrs2] = [MarkdownAstNode item sr (attrs1 ++ attrs2)]
  addAttributes attrs1 xs = [MarkdownAstNode (Span xs) Nothing attrs1]

instance Rangeable MarkdownAst where
  ranged _ [] = []
  ranged sr [MarkdownAstNode item _ attrs] = [MarkdownAstNode item (Just sr) attrs]
  ranged sr xs = [MarkdownAstNode (Span xs) (Just sr) []]

instance IsInline MarkdownAst where
  lineBreak = rawNode LineBreak
  softBreak = rawNode SoftBreak
  entity = rawNode . Entity
  emph = rawNode . Emphasis
  strong = rawNode . Strong
  link target title inline = rawNode $ Link $ LinkData target title inline
  image source title inline = rawNode $ Image $ ImageData source title inline
  code x = rawNode $ Code x
  rawInline format x = rawNode $ RawInline format x
  str = rawNode . Text
  escapedChar = rawNode . EscapedChar

instance IsBlock MarkdownAst MarkdownAst where
  paragraph = rawNode . Paragraph
  plain = rawNode . Plain
  thematicBreak = rawNode HorizontalRule
  blockQuote = rawNode . Blockquote
  codeBlock info t = rawNode $ CodeBlock info t
  heading level il = rawNode $ Header level il
  rawBlock format t = rawNode $ RawBlock format t
  list listtype spacing items = rawNode $ List $ ListData listtype spacing items
  referenceLinkDefinition label (dest, title) = rawNode $ ReferenceLinkDefination $ ReferenceLinkDefinationData label dest title

instance HasMath MarkdownAst where
  inlineMath x = rawNode $ InlineMath x
  displayMath x = rawNode $ DisplayMath x

instance HasEmoji MarkdownAst where
  emoji x y = rawNode $ Emoji x y

instance HasSubscript MarkdownAst where
  subscript = rawNode . Subscript

instance HasSuperscript MarkdownAst where
  superscript = rawNode . Superscript

instance HasStrikethrough MarkdownAst where
  strikethrough = rawNode . Strikethrough

instance HasPipeTable MarkdownAst MarkdownAst where
  pipeTable align header rows = rawNode $ PipeTable align header rows

instance HasDefinitionList MarkdownAst MarkdownAst where
  definitionList spacing xs = rawNode $ DefinitionList spacing xs

instance HasTaskList MarkdownAst MarkdownAst where
  taskList listtype spacing items = rawNode (TaskList listtype spacing items)

instance HasAlerts MarkdownAst MarkdownAst where
  alert a = rawNode . Alert a

instance HasWikilinks MarkdownAst where
  wikilink target inline = rawNode $ WikiLink $ WikiLinkData target inline

instance HasFootnote MarkdownAst MarkdownAst where
  footnote num label bl = rawNode $ Footnote num label bl
  footnoteList items = rawNode $ FootnoteList items
  footnoteRef num label = rawNode . FootnoteRef num label

instance HasQuoted MarkdownAst where
  singleQuoted = rawNode . SingleQuoted
  doubleQuoted = rawNode . DoubleQuoted

instance HasSpan MarkdownAst where
  spanWith _ [] = []
  spanWith attrs1 [MarkdownAstNode item sr attrs2] = [MarkdownAstNode item sr (attrs1 ++ attrs2)]
  spanWith attrs1 xs = [MarkdownAstNode (Span xs) Nothing attrs1]

instance HasDiv MarkdownAst where
  div_ = id

toPlainTextBuilder' :: MarkdownAstNode -> TB.Builder
toPlainTextBuilder' (MarkdownAstNode ele _ _) = case ele of
  Text t -> TB.fromText t
  Entity t -> TB.fromText t
  LineBreak -> "\n"
  SoftBreak -> " "
  EscapedChar c -> TB.singleton c
  Code t -> TB.fromText t
  Emphasis ast -> toPlainTextBuilder ast
  Strong ast -> toPlainTextBuilder ast
  Link link -> toPlainTextBuilder $ link ^. linkInline
  Image image -> toPlainTextBuilder $ image ^. imageInline
  Strikethrough ast -> toPlainTextBuilder ast
  Highlight ast -> toPlainTextBuilder ast
  RawInline _ t -> TB.fromText t
  Emoji _ t -> TB.fromText t
  InlineMath t -> TB.fromText t
  DisplayMath t -> TB.fromText t
  SingleQuoted ast -> "\'" <> toPlainTextBuilder ast <> "\'"
  DoubleQuoted ast -> "\"" <> toPlainTextBuilder ast <> "\""
  Subscript ast -> " " <> toPlainTextBuilder ast <> " "
  Superscript ast -> " " <> toPlainTextBuilder ast <> " "
  Paragraph ast -> toPlainTextBuilder ast <> "\n"
  Plain ast -> toPlainTextBuilder ast
  Alert a ast -> TB.fromText (alertName a) <> ": " <> toPlainTextBuilder ast
  Header _ ast -> toPlainTextBuilder ast <> "\n"
  List list -> mconcat $ map (\ast -> toPlainTextBuilder ast <> "\n") $ list ^. listItems
  Blockquote ast -> toPlainTextBuilder ast <> "\n"
  CodeBlock _ t -> TB.fromText t <> "\n"
  RawBlock _ t -> TB.fromText t <> "\n"
  HorizontalRule -> "\n\n"
  PipeTable _ title rows -> tableHelper (title : rows) <> "\n"
    where
      tableHelper [] = ""
      tableHelper (x : xs) =
        mconcat (map (\y -> toPlainTextBuilder y <> " ") x) <> "\n" <> tableHelper xs
  ReferenceLinkDefination ref -> TB.fromText $ ref ^. referenceLinkDefinationLabel
  DefinitionList _ asts ->
    mconcat
      ( map
          ( \(term, defs) ->
              toPlainTextBuilder term
                <> "\n"
                <> mconcat
                  ( map (\def -> toPlainTextBuilder def <> "\n") defs
                  )
          )
          asts
      )
  TaskList _ _ items -> mconcat $ map (\(_, ast) -> toPlainTextBuilder ast <> "\n") items
  WikiLink wiki -> toPlainTextBuilder (wiki ^. wikiLinkInline)
  Footnote num txt ast ->
    "[" <> TB.fromString (show num) <> "] " <> TB.fromText txt <> toPlainTextBuilder ast
  FootnoteList items -> mconcat $ map (\item -> toPlainTextBuilder item <> "\n") items
  FootnoteRef num _ _ -> "[" <> TB.fromString (show num) <> "] "
  Placeholder _ -> ""
  Span asts -> mconcat $ map toPlainTextBuilder' asts

toPlainTextBuilder :: MarkdownAst -> TB.Builder
toPlainTextBuilder = foldr ((<>) . toPlainTextBuilder') ""

instance ToPlainText MarkdownAst where
  toPlainText = LT.toStrict . TB.toLazyText . toPlainTextBuilder

instance HasPlaceholder MarkdownAst where
  placeholder = rawNode . Placeholder

children :: MarkdownElement -> [MarkdownAst]
children (Emphasis ast) = [ast]
children (Strong ast) = [ast]
children (Strikethrough ast) = [ast]
children (Superscript ast) = [ast]
children (Subscript ast) = [ast]
children (Link link) = [link ^. linkInline]
children (Image image) = [image ^. imageInline]
children (Span asts) = [asts]
children (Paragraph ast) = [ast]
children (Plain ast) = [ast]
children (Header _ ast) = [ast]
children (List list) = list ^. listItems
children (Blockquote ast) = [ast]
children (PipeTable _ title rows) = concat (title : rows)
children (DefinitionList _ asts) = map fst asts ++ concatMap snd asts
children (TaskList _ _ items) = map snd items
children (WikiLink wiki) = [wiki ^. wikiLinkInline]
children (Footnote _ _ ast) = [ast]
children (FootnoteList items) = items
children (FootnoteRef _ _ ast) = [ast]
children (Alert _ ast) = [ast]
children _ = []

findPlaceholders :: MarkdownAst -> [(T.Text, SourceRange)]
findPlaceholders = concatMap \case
  (MarkdownAstNode (Placeholder txt) srange _) -> maybeToList (fmap (txt,) srange)
  (MarkdownAstNode ele _ _) -> concatMap findPlaceholders (children ele)

findLinks :: MarkdownAst -> [(T.Text, T.Text)]
findLinks = concatMap \case
  (MarkdownAstNode (Link (LinkData target title _)) _ _) -> [(target, title)]
  (MarkdownAstNode ele _ _) -> concatMap findLinks (children ele)

findTasks :: MarkdownAst -> [(Bool, MarkdownAst)]
findTasks = concatMap \case
  (MarkdownAstNode (TaskList _ _ items) _ _) -> items
  (MarkdownAstNode ele _ _) -> concatMap findTasks (children ele)

findFinishedTasks :: MarkdownAst -> [MarkdownAst]
findFinishedTasks = map snd . filter fst . findTasks

findUnfinishedTasks :: MarkdownAst -> [MarkdownAst]
findUnfinishedTasks = map snd . filter (not . fst) . findTasks

findAlerts :: MarkdownAst -> [(AlertType, MarkdownAst)]
findAlerts = concatMap \case
  (MarkdownAstNode (Alert t ast) _ _) -> [(t, ast)]
  (MarkdownAstNode ele _ _) -> concatMap findAlerts (children ele)

betweenPos :: (Integral a) => a -> a -> (SourcePos, SourcePos) -> Bool
betweenPos r c (s1, s2)
  | row == r1 && row == r2 = c1 <= col && col < c2
  | row == r1 = c1 <= col
  | row == r2 = col < c2
  | r1 < row && row < r2 = True
  | otherwise = False
  where
    row = fromIntegral r
    col = fromIntegral c
    r1 = sourceLine s1
    c1 = sourceColumn s1
    r2 = sourceLine s2
    c2 = sourceColumn s2

inRange :: (Integral a) => a -> a -> SourceRange -> Bool
inRange row col sr = any (betweenPos row col) (unSourceRange sr)

allNodes' :: (MarkdownAstNode -> Maybe a) -> MarkdownAst -> [a]
allNodes' f = concatMap (helper f)
  where
    helper f node =
      case f node of
        Just x -> [x]
        Nothing -> concatMap (allNodes' f) $ children $ node ^. element

allNodes :: (MarkdownAstNode -> Bool) -> MarkdownAst -> [MarkdownAstNode]
allNodes f = concatMap (helper f)
  where
    helper f node = if f node then [node] else concatMap (allNodes f) (children $ node ^. element)

firstNode :: (MarkdownAstNode -> Bool) -> MarkdownAst -> Maybe MarkdownAstNode
firstNode f ast = listToMaybe (allNodes f ast)

firstNode' :: (MarkdownAstNode -> Maybe a) -> MarkdownAst -> Maybe a
firstNode' f = listToMaybe . allNodes' f

nodeAt :: (Integral i) => (MarkdownAstNode -> Bool) -> i -> i -> MarkdownAst -> Maybe MarkdownAstNode
nodeAt f row col =
  firstNode \node -> case node ^. sourceRange of
    Just sr -> inRange row col sr && f node
    _ -> False

nodeAt' :: (Integral i) => (MarkdownAstNode -> Maybe a) -> i -> i -> MarkdownAst -> Maybe a
nodeAt' f row col = firstNode' \node -> do
  sr <- node ^. sourceRange
  guard $ inRange row col sr
  f node
