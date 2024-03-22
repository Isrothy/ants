{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeInType #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Model.MarkdownAst
  ( MarkdownAst,
    MarkdownElement (..),
    AstNode (..),
    MdNode,
    parameters,
    sourceRange,
    attributes,
    findPlaceholders,
    findLinks,
    findTasks,
    findAlerts,
    findHaders,
    allNodes,
    firstNode,
    nodeAt,
    firstNode',
    allNodes',
    nodeAt',
  )
where

import Commonmark
import Commonmark.Extensions
import Control.Lens ((^.))
import Control.Lens.TH (makeLenses)
import Control.Monad
import Data.Maybe
import qualified Data.Text.Internal.Builder as TB
import qualified Data.Text.Lazy as LT
import Model.MarkdownAst.Lenses
import Model.MarkdownAst.Params.AlertParams
import Model.MarkdownAst.Params.BlockquoteParams
import Model.MarkdownAst.Params.CodeBlockParams
import Model.MarkdownAst.Params.CodeParams
import Model.MarkdownAst.Params.DefinitionListParams
import Model.MarkdownAst.Params.DisplayMathParams
import Model.MarkdownAst.Params.DoubleQuotedParams
import Model.MarkdownAst.Params.EmojiParams
import Model.MarkdownAst.Params.EmphasisParams
import Model.MarkdownAst.Params.EntityParams
import Model.MarkdownAst.Params.EscapedCharParams
import Model.MarkdownAst.Params.FootnoteListParams
import Model.MarkdownAst.Params.FootnoteParams
import Model.MarkdownAst.Params.FootnoteRefParams
import Model.MarkdownAst.Params.HeaderParams
import Model.MarkdownAst.Params.HighlightParams
import Model.MarkdownAst.Params.HorizontalRuleParams
import Model.MarkdownAst.Params.ImageParams
import Model.MarkdownAst.Params.InlineMathParams
import Model.MarkdownAst.Params.LineBreakParams
import Model.MarkdownAst.Params.LinkParams
import Model.MarkdownAst.Params.ListParams
import Model.MarkdownAst.Params.ParagraphParams
import Model.MarkdownAst.Params.PipeTableParams
import Model.MarkdownAst.Params.PlaceholderParams
import Model.MarkdownAst.Params.PlainParams
import Model.MarkdownAst.Params.RawBlockParams
import Model.MarkdownAst.Params.RawInlineParams
import Model.MarkdownAst.Params.ReferenceLinkDefinationParams
import Model.MarkdownAst.Params.SingleQuotedParams
import Model.MarkdownAst.Params.SoftBreakParams
import Model.MarkdownAst.Params.SpanParams
import Model.MarkdownAst.Params.StrikethroughParams
import Model.MarkdownAst.Params.StrongParams
import Model.MarkdownAst.Params.SubscriptParams
import Model.MarkdownAst.Params.SuperscriptParams
import Model.MarkdownAst.Params.TaskListParams
import Model.MarkdownAst.Params.TextParams
import Model.MarkdownAst.Params.WikiLinkParams
import Parser.Placeholder

data AstNode p where
  AstNode ::
    { _parameters :: p,
      _sourceRange :: Maybe SourceRange,
      _attributes :: Attributes
    } ->
    AstNode p
  deriving (Show, Eq)

type MdNode = AstNode MarkdownElement

type MarkdownAst = [MdNode]

data MarkdownElement where
  Alert :: (AlertParams MarkdownAst MarkdownAst) -> MarkdownElement
  Blockquote :: (BlockquoteParams MarkdownAst MarkdownAst) -> MarkdownElement
  Code :: CodeParams -> MarkdownElement
  CodeBlock :: CodeBlockParams -> MarkdownElement
  DefinitionList :: (DefinitionListParams MarkdownAst MarkdownAst) -> MarkdownElement
  DisplayMath :: DisplayMathParams -> MarkdownElement
  DoubleQuoted :: (DoubleQuotedParams MarkdownAst) -> MarkdownElement
  Emoji :: EmojiParams -> MarkdownElement
  Emphasis :: (EmphasisParams MarkdownAst) -> MarkdownElement
  Entity :: EntityParams -> MarkdownElement
  EscapedChar :: EscapedCharParams -> MarkdownElement
  Footnote :: (FootnoteParams MarkdownAst MarkdownAst) -> MarkdownElement
  FootnoteList :: (FootnoteListParams MarkdownAst MarkdownAst) -> MarkdownElement
  FootnoteRef :: (FootnoteRefParams MarkdownAst) -> MarkdownElement
  Header :: (HeaderParams MarkdownAst) -> MarkdownElement
  Highlight :: (HighlightParams MarkdownAst) -> MarkdownElement
  HorizontalRule :: HorizontalRuleParams -> MarkdownElement
  Image :: (ImageParams MarkdownAst) -> MarkdownElement
  InlineMath :: InlineMathParams -> MarkdownElement
  LineBreak :: LineBreakParams -> MarkdownElement
  Link :: (LinkParams MarkdownAst) -> MarkdownElement
  List :: (ListParams MarkdownAst MarkdownAst) -> MarkdownElement
  Paragraph :: (ParagraphParams MarkdownAst) -> MarkdownElement
  PipeTable :: (PipeTableParams MarkdownAst) -> MarkdownElement
  Placeholder :: PlaceholderParams -> MarkdownElement
  Plain :: (PlainParams MarkdownAst) -> MarkdownElement
  RawBlock :: RawBlockParams -> MarkdownElement
  RawInline :: RawInlineParams -> MarkdownElement
  ReferenceLinkDefination :: ReferenceLinkDefinationParams -> MarkdownElement
  SingleQuoted :: (SingleQuotedParams MarkdownAst) -> MarkdownElement
  SoftBreak :: SoftBreakParams -> MarkdownElement
  Span :: (SpanParams MarkdownAst) -> MarkdownElement
  Strikethrough :: (StrikethroughParams MarkdownAst) -> MarkdownElement
  Strong :: (StrongParams MarkdownAst) -> MarkdownElement
  Subscript :: (SubscriptParams MarkdownAst) -> MarkdownElement
  Superscript :: (SuperscriptParams MarkdownAst) -> MarkdownElement
  TaskList :: (TaskListParams MarkdownAst MarkdownAst) -> MarkdownElement
  Text :: TextParams -> MarkdownElement
  WikiLink :: (WikiLinkParams MarkdownAst) -> MarkdownElement
  deriving (Show, Eq)

makeLenses ''AstNode

rawNode :: MarkdownElement -> [MdNode]
rawNode x = [AstNode x Nothing []]

instance HasAttributes MarkdownAst where
  addAttributes _ [] = []
  addAttributes attrs1 [AstNode item sr attrs2] = [AstNode item sr (attrs1 ++ attrs2)]
  addAttributes attrs1 xs = [AstNode (Span (SpanParams xs)) Nothing attrs1]

instance Rangeable MarkdownAst where
  ranged _ [] = []
  ranged sr [AstNode item _ attrs] = [AstNode item (Just sr) attrs]
  ranged sr xs = [AstNode (Span (SpanParams xs)) (Just sr) []]

instance IsInline MarkdownAst where
  lineBreak = rawNode $ LineBreak LineBreakParams
  softBreak = rawNode $ SoftBreak SoftBreakParams
  entity = rawNode . Entity . EntityParams
  emph = rawNode . Emphasis . EmphasisParams
  strong = rawNode . Strong . StrongParams
  link target title inline = rawNode $ Link $ LinkParams target title inline
  image source title inline = rawNode $ Image $ ImageParams source title inline
  code = rawNode . Code . CodeParams
  rawInline format x = rawNode $ RawInline $ RawInlineParams format x
  str = rawNode . Text . TextParams
  escapedChar = rawNode . EscapedChar . EscapedCharParams

instance IsBlock MarkdownAst MarkdownAst where
  paragraph = rawNode . Paragraph . ParagraphParams
  plain = rawNode . Plain . PlainParams
  thematicBreak = rawNode $ HorizontalRule HorizontalRuleParams
  blockQuote ast = rawNode $ Blockquote $ BlockquoteParams ast
  codeBlock info t = rawNode $ CodeBlock $ CodeBlockParams info t
  heading level il = rawNode $ Header $ HeaderParams level il
  rawBlock format t = rawNode $ RawBlock $ RawBlockParams format t
  list listtype spacing items = rawNode $ List $ ListData listtype spacing items
  referenceLinkDefinition label (dest, title) = rawNode $ ReferenceLinkDefination $ ReferenceLinkDefinationData label dest title

instance HasMath MarkdownAst where
  inlineMath x = rawNode $ InlineMath $ InlineMathParams x
  displayMath x = rawNode $ DisplayMath $ DisplayMathParams x

instance Commonmark.Extensions.HasEmoji MarkdownAst where
  emoji x y = rawNode $ Emoji $ EmojiParams x y

instance HasSubscript MarkdownAst where
  subscript = rawNode . Subscript . SubscriptParams

instance HasSuperscript MarkdownAst where
  superscript = rawNode . Superscript . SuperscriptParams

instance HasStrikethrough MarkdownAst where
  strikethrough = rawNode . Strikethrough . StrikethroughParams

instance HasPipeTable MarkdownAst MarkdownAst where
  pipeTable align header rows = rawNode $ PipeTable $ PipeTableParams align header rows

instance HasDefinitionList MarkdownAst MarkdownAst where
  definitionList spacing xs = rawNode $ DefinitionList $ DefinitionListParams spacing xs

instance HasTaskList MarkdownAst MarkdownAst where
  taskList listtype spacing items = rawNode $ TaskList $ TaskListParams listtype spacing items

instance HasAlerts MarkdownAst MarkdownAst where
  alert a = rawNode . Alert . AlertParams a

instance HasWikilinks MarkdownAst where
  wikilink target inline = rawNode $ WikiLink $ WikiLinkParams target inline

instance HasFootnote MarkdownAst MarkdownAst where
  footnote num label bl = rawNode $ Footnote $ FootnoteParams num label bl
  footnoteList items = rawNode $ FootnoteList $ FootnoteListParams items
  footnoteRef num label bl = rawNode $ FootnoteRef $ FootnoteRefParams num label bl

instance HasQuoted MarkdownAst where
  singleQuoted = rawNode . SingleQuoted . SingleQuotedParams
  doubleQuoted = rawNode . DoubleQuoted . DoubleQuotedParams

instance HasSpan MarkdownAst where
  spanWith _ [] = []
  spanWith attrs1 [AstNode item sr attrs2] = [AstNode item sr (attrs1 ++ attrs2)]
  spanWith attrs1 xs = [AstNode (Span (SpanParams xs)) Nothing attrs1]

instance HasDiv MarkdownAst where
  div_ = id

class HasPlainTextBuilder x where
  toPlainTextBuilder :: x -> TB.Builder

instance HasPlainTextBuilder MdNode where
  toPlainTextBuilder (AstNode ele _ _) = case ele of
    Text params -> TB.fromText $ params ^. text
    Entity params -> TB.fromText $ params ^. text
    LineBreak _ -> "\n"
    SoftBreak _ -> " "
    EscapedChar params -> TB.singleton $ params ^. char
    Code params -> TB.fromText $ params ^. text
    Emphasis params -> toPlainTextBuilder $ params ^. inline
    Strong params -> toPlainTextBuilder $ params ^. inline
    Link params -> toPlainTextBuilder $ params ^. inline
    Image params -> toPlainTextBuilder $ params ^. inline
    Strikethrough params -> toPlainTextBuilder $ params ^. inline
    Highlight params -> toPlainTextBuilder $ params ^. inline
    RawInline params -> TB.fromText $ params ^. text
    Emoji params -> TB.fromText $ params ^. Model.MarkdownAst.Lenses.emoji
    InlineMath params -> TB.fromText $ params ^. text
    DisplayMath params -> TB.fromText $ params ^. text
    SingleQuoted params -> "\'" <> toPlainTextBuilder (params ^. inline) <> "\'"
    DoubleQuoted params -> "\"" <> toPlainTextBuilder (params ^. inline) <> "\""
    Subscript params -> " " <> toPlainTextBuilder (params ^. inline) <> " "
    Superscript params -> " " <> toPlainTextBuilder (params ^. inline) <> " "
    Paragraph params -> toPlainTextBuilder (params ^. inline) <> "\n"
    Plain params -> toPlainTextBuilder (params ^. inline)
    Alert params ->
      TB.fromText
        (alertName (params ^. alertType))
        <> ": "
        <> toPlainTextBuilder (params ^. block)
    Header params -> toPlainTextBuilder (params ^. inline) <> "\n"
    List params -> mconcat $ map (\ast -> toPlainTextBuilder ast <> "\n") $ params ^. listItems
    Blockquote params -> toPlainTextBuilder (params ^. block) <> "\n"
    CodeBlock params -> TB.fromText (params ^. text) <> "\n"
    RawBlock params -> TB.fromText (params ^. text) <> "\n"
    HorizontalRule _ -> "\n\n"
    PipeTable params -> tableHelper ((params ^. headers) : (params ^. rows)) <> "\n"
      where
        tableHelper [] = ""
        tableHelper (x : xs) =
          mconcat (map (\y -> toPlainTextBuilder y <> " ") x) <> "\n" <> tableHelper xs
    ReferenceLinkDefination params -> TB.fromText $ params ^. label
    DefinitionList params ->
      mconcat
        ( map
            ( \(term, defs) ->
                toPlainTextBuilder term
                  <> "\n"
                  <> mconcat
                    ( map (\def -> toPlainTextBuilder def <> "\n") defs
                    )
            )
            (params ^. defListItems)
        )
    TaskList params -> mconcat $ map (\(_, ast) -> toPlainTextBuilder ast <> "\n") (params ^. taskListItems)
    WikiLink wiki -> toPlainTextBuilder (wiki ^. inline)
    Footnote params ->
      "[" <> TB.fromString (show (params ^. number)) <> "] " <> TB.fromText (params ^. label) <> toPlainTextBuilder (params ^. block)
    FootnoteList params -> mconcat $ map (\item -> toPlainTextBuilder item <> "\n") (params ^. listItems)
    FootnoteRef (FootnoteRefParams num _ _) -> "[" <> TB.fromString (show num) <> "] "
    Placeholder _ -> ""
    Span (SpanParams asts) -> mconcat $ map toPlainTextBuilder asts

instance HasPlainTextBuilder MarkdownAst where
  toPlainTextBuilder = foldr ((<>) . toPlainTextBuilder) ""

instance ToPlainText MarkdownAst where
  toPlainText = LT.toStrict . TB.toLazyText . toPlainTextBuilder

instance HasPlaceholder MarkdownAst where
  placeholder = rawNode . Placeholder . PlaceholderParams

children :: MarkdownElement -> [MarkdownAst]
children (Emphasis params) = [params ^. inline]
children (Strong params) = [params ^. inline]
children (Strikethrough params) = [params ^. inline]
children (Superscript params) = [params ^. inline]
children (Subscript params) = [params ^. inline]
children (Link link) = [link ^. inline]
children (Image image) = [image ^. inline]
children (Span (SpanParams children)) = [children]
children (List list) = list ^. listItems
children (Blockquote params) = [params ^. block]
children (TaskList params) = map snd (params ^. taskListItems)
children (WikiLink params) = [params ^. inline]
children (Footnote params) = [params ^. block]
children (FootnoteList params) = params ^. listItems
children (Alert alert) = [alert ^. block]
children (Paragraph params) = [params ^. inline]
children (Plain params) = [params ^. inline]
children (Header params) = [params ^. inline]
children (PipeTable params) = concat (params ^. headers : params ^. rows)
children (DefinitionList params) = map fst (params ^. defListItems) ++ concatMap snd (params ^. defListItems)
children _ = []

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

allNodes' :: (MdNode -> Maybe a) -> MarkdownAst -> [a]
allNodes' f = concatMap (helper f)
  where
    helper f node =
      case f node of
        Just x -> [x]
        Nothing -> concatMap (allNodes' f) $ children $ node ^. parameters

allNodes :: (MdNode -> Bool) -> MarkdownAst -> [MdNode]
allNodes f = concatMap (helper f)
  where
    helper f node = if f node then [node] else concatMap (allNodes f) (children $ node ^. parameters)

firstNode :: (MdNode -> Bool) -> MarkdownAst -> Maybe MdNode
firstNode f ast = listToMaybe (allNodes f ast)

firstNode' :: (MdNode -> Maybe a) -> MarkdownAst -> Maybe a
firstNode' f = listToMaybe . allNodes' f

nodeAt :: (Integral i) => (MdNode -> Bool) -> i -> i -> MarkdownAst -> Maybe MdNode
nodeAt f row col =
  firstNode \node -> case node ^. sourceRange of
    Just sr -> inRange row col sr && f node
    _ -> False

nodeAt' :: (Integral i) => (MdNode -> Maybe a) -> i -> i -> MarkdownAst -> Maybe a
nodeAt' f row col = firstNode' \node -> do
  sr <- node ^. sourceRange
  guard $ inRange row col sr
  f node

findPlaceholders :: MarkdownAst -> [AstNode PlaceholderParams]
findPlaceholders = allNodes' \case
  (AstNode (Placeholder params) sr attr) -> Just (AstNode params sr attr)
  _ -> Nothing

findLinks :: MarkdownAst -> [AstNode (LinkParams MarkdownAst)]
findLinks = allNodes' \case
  (AstNode (Link params) sr attr) -> Just (AstNode params sr attr)
  _ -> Nothing

findTasks :: MarkdownAst -> [AstNode (TaskListParams MarkdownAst MarkdownAst)]
findTasks = allNodes' \case
  (AstNode (TaskList params) sr attr) -> Just (AstNode params sr attr)
  _ -> Nothing

findAlerts :: MarkdownAst -> [AstNode (AlertParams MarkdownAst MarkdownAst)]
findAlerts = allNodes' \case
  (AstNode (Alert params) sr attr) -> Just (AstNode params sr attr)
  _ -> Nothing

findHaders :: MarkdownAst -> [AstNode (HeaderParams MarkdownAst)]
findHaders = allNodes' \case
  (AstNode (Header params) sr attr) -> Just (AstNode params sr attr)
  _ -> Nothing
