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
    InlineAst,
    BlockAst,
    MarkdownElement (..),
    AstNode (..),
    MarkdownAstKind (..),
    MdNode,
    paramaters,
    sourceRange,
    attributes,
    findPlaceholders,
    findLinks,
    findTasks,
    findAlerts,
    firstNode,
    findHaders,
    allNodes,
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
import Model.MarkdownAst.Kinds
import Model.MarkdownAst.Lenses
import Model.MarkdownAst.Params.AlertParams
import Model.MarkdownAst.Params.BlockquoteParams
import Model.MarkdownAst.Params.CodeBlockParams
import Model.MarkdownAst.Params.CodeParams
import Model.MarkdownAst.Params.DefinitionListParams
import Model.MarkdownAst.Params.DisplayMathParams
import Model.MarkdownAst.Params.DivParams
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
    { _paramaters :: p,
      _sourceRange :: Maybe SourceRange,
      _attributes :: Attributes
    } ->
    AstNode p
  deriving (Show, Eq)

type MdNode (k :: MarkdownAstKind) = AstNode (MarkdownElement k)

type MarkdownAst = BlockAst

type GenericAst k = [MdNode k]

type InlineAst = GenericAst Il

type BlockAst = GenericAst Bl

data MarkdownElement (k :: MarkdownAstKind) where
  Alert :: (AlertParams InlineAst BlockAst) -> MarkdownElement Bl
  Blockquote :: (BlockquoteParams InlineAst BlockAst) -> MarkdownElement Bl
  Code :: CodeParams -> MarkdownElement Il
  CodeBlock :: CodeBlockParams -> MarkdownElement Bl
  DefinitionList :: (DefinitionListParams InlineAst BlockAst) -> MarkdownElement Bl
  DisplayMath :: DisplayMathParams -> MarkdownElement Il
  Div :: (DivParams InlineAst BlockAst) -> MarkdownElement Bl
  DoubleQuoted :: (DoubleQuotedParams InlineAst) -> MarkdownElement Il
  Emoji :: EmojiParams -> MarkdownElement Il
  Emphasis :: (EmphasisParams InlineAst) -> MarkdownElement Il
  Entity :: EntityParams -> MarkdownElement Il
  EscapedChar :: EscapedCharParams -> MarkdownElement Il
  Footnote :: (FootnoteParams InlineAst BlockAst) -> MarkdownElement Bl
  FootnoteList :: (FootnoteListParams InlineAst BlockAst) -> MarkdownElement Bl
  FootnoteRef :: (FootnoteRefParams BlockAst) -> MarkdownElement Il
  Header :: (HeaderParams InlineAst) -> MarkdownElement Bl
  Highlight :: (HighlightParams InlineAst) -> MarkdownElement Il
  HorizontalRule :: HorizontalRuleParams -> MarkdownElement Bl
  Image :: (ImageParams InlineAst) -> MarkdownElement Il
  InlineMath :: InlineMathParams -> MarkdownElement Il
  LineBreak :: LineBreakParams -> MarkdownElement Il
  Link :: (LinkParams InlineAst) -> MarkdownElement Il
  List :: (ListParams InlineAst BlockAst) -> MarkdownElement Bl
  Paragraph :: (ParagraphParams InlineAst) -> MarkdownElement Bl
  PipeTable :: (PipeTableParams InlineAst) -> MarkdownElement Bl
  Placeholder :: PlaceholderParams -> MarkdownElement Il
  Plain :: (PlainParams InlineAst) -> MarkdownElement Bl
  RawBlock :: RawBlockParams -> MarkdownElement Bl
  RawInline :: RawInlineParams -> MarkdownElement Il
  ReferenceLinkDefination :: ReferenceLinkDefinationParams -> MarkdownElement Bl
  SingleQuoted :: (SingleQuotedParams InlineAst) -> MarkdownElement Il
  SoftBreak :: SoftBreakParams -> MarkdownElement Il
  Span :: (SpanParams InlineAst) -> MarkdownElement Il
  Strikethrough :: (StrikethroughParams InlineAst) -> MarkdownElement Il
  Strong :: (StrongParams InlineAst) -> MarkdownElement Il
  Subscript :: (SubscriptParams InlineAst) -> MarkdownElement Il
  Superscript :: (SuperscriptParams InlineAst) -> MarkdownElement Il
  TaskList :: (TaskListParams InlineAst BlockAst) -> MarkdownElement Bl
  Text :: TextParams -> MarkdownElement Il
  WikiLink :: (WikiLinkParams InlineAst) -> MarkdownElement Il

instance Show (MarkdownElement Il) where
  show (Code params) = "Code " ++ show params
  show (DisplayMath params) = "DisplayMath " ++ show params
  show (DoubleQuoted params) = "DoubleQuoted " ++ show params
  show (Emoji params) = "Emoji " ++ show params
  show (Emphasis params) = "Emphasis " ++ show params
  show (Entity params) = "Entity " ++ show params
  show (EscapedChar params) = "EscapedChar " ++ show params
  show (FootnoteRef params) = "FootnoteRef " ++ show params
  show (Highlight params) = "Highlight " ++ show params
  show (Image params) = "Image " ++ show params
  show (InlineMath params) = "InlineMath " ++ show params
  show (LineBreak params) = "LineBreak " ++ show params
  show (Link params) = "Link " ++ show params
  show (Placeholder params) = "Placeholder " ++ show params
  show (RawInline params) = "RawInline " ++ show params
  show (SingleQuoted params) = "SingleQuoted " ++ show params
  show (SoftBreak params) = "SoftBreak " ++ show params
  show (Span params) = "Span " ++ show params
  show (Strikethrough params) = "Strikethrough " ++ show params
  show (Strong params) = "Strong " ++ show params
  show (Subscript params) = "Subscript " ++ show params
  show (Superscript params) = "Superscript " ++ show params
  show (Text params) = "Text " ++ show params
  show (WikiLink params) = "WikiLink " ++ show params

instance Show (MarkdownElement Bl) where
  show (Alert params) = "Alert " ++ show params
  show (Blockquote params) = "Blockquote " ++ show params
  show (CodeBlock params) = "CodeBlock " ++ show params
  show (DefinitionList params) = "DefinitionList " ++ show params
  show (FootnoteList params) = "FootnoteList " ++ show params
  show (Header params) = "Header " ++ show params
  show (HorizontalRule params) = "HorizontalRule " ++ show params
  show (List params) = "List " ++ show params
  show (Paragraph params) = "Paragraph " ++ show params
  show (PipeTable params) = "PipeTable " ++ show params
  show (Plain params) = "Plain " ++ show params
  show (Footnote params) = "Footnote " ++ show params
  show (RawBlock params) = "RawBlock " ++ show params
  show (ReferenceLinkDefination params) = "ReferenceLinkDefination " ++ show params
  show (TaskList params) = "TaskList " ++ show params
  show (Div params) = "Div " ++ show params

instance Eq (MarkdownElement Il) where
  Code x == Code y = x == y
  DoubleQuoted x == DoubleQuoted y = x == y
  Emoji x == Emoji y = x == y
  DisplayMath x == DisplayMath y = x == y
  Emphasis x == Emphasis y = x == y
  Entity x == Entity y = x == y
  EscapedChar x == EscapedChar y = x == y
  FootnoteRef x == FootnoteRef y = x == y
  Highlight x == Highlight y = x == y
  Image x == Image y = x == y
  InlineMath x == InlineMath y = x == y
  LineBreak x == LineBreak y = x == y
  Link x == Link y = x == y
  Placeholder x == Placeholder y = x == y
  RawInline x == RawInline y = x == y
  SingleQuoted x == SingleQuoted y = x == y
  SoftBreak x == SoftBreak y = x == y
  Span x == Span y = x == y
  Strikethrough x == Strikethrough y = x == y
  Strong x == Strong y = x == y
  Subscript x == Subscript y = x == y
  Superscript x == Superscript y = x == y
  Text x == Text y = x == y
  WikiLink x == WikiLink y = x == y
  _ == _ = False

instance Eq (MarkdownElement Bl) where
  Alert x == Alert y = x == y
  Blockquote x == Blockquote y = x == y
  CodeBlock x == CodeBlock y = x == y
  DefinitionList x == DefinitionList y = x == y
  FootnoteList x == FootnoteList y = x == y
  Header x == Header y = x == y
  HorizontalRule x == HorizontalRule y = x == y
  List x == List y = x == y
  Paragraph x == Paragraph y = x == y
  PipeTable x == PipeTable y = x == y
  Footnote x == Footnote y = x == y
  Plain x == Plain y = x == y
  RawBlock x == RawBlock y = x == y
  ReferenceLinkDefination x == ReferenceLinkDefination y = x == y
  TaskList x == TaskList y = x == y
  Div x == Div y = x == y
  _ == _ = False

makeLenses ''AstNode

rawNode :: MarkdownElement (k :: MarkdownAstKind) -> [MdNode k]
rawNode x = [AstNode x Nothing []]

instance HasAttributes InlineAst where
  addAttributes _ [] = []
  addAttributes attrs1 [AstNode item sr attrs2] = [AstNode item sr (attrs1 ++ attrs2)]
  addAttributes attrs1 xs = [AstNode (Span (SpanParams xs)) Nothing attrs1]

instance HasAttributes BlockAst where
  addAttributes _ [] = []
  addAttributes attrs1 [AstNode item sr attrs2] = [AstNode item sr (attrs1 ++ attrs2)]
  addAttributes attrs1 xs = [AstNode (Div (DivParams xs)) Nothing attrs1]

instance Rangeable InlineAst where
  ranged _ [] = []
  ranged sr [AstNode item _ attrs] = [AstNode item (Just sr) attrs]
  ranged sr xs = [AstNode (Span (SpanParams xs)) (Just sr) []]

instance Rangeable BlockAst where
  ranged _ [] = []
  ranged sr [AstNode item _ attrs] = [AstNode item (Just sr) attrs]
  ranged sr xs = [AstNode (Div (DivParams xs)) (Just sr) []]

instance IsInline InlineAst where
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

instance IsBlock InlineAst BlockAst where
  paragraph = rawNode . Paragraph . ParagraphParams
  plain = rawNode . Plain . PlainParams
  thematicBreak = rawNode $ HorizontalRule HorizontalRuleParams
  blockQuote ast = rawNode $ Blockquote $ BlockquoteParams ast
  codeBlock info t = rawNode $ CodeBlock $ CodeBlockParams info t
  heading level il = rawNode $ Header $ HeaderParams level il
  rawBlock format t = rawNode $ RawBlock $ RawBlockParams format t
  list listtype spacing items = rawNode $ List $ ListData listtype spacing items
  referenceLinkDefinition label (dest, title) = rawNode $ ReferenceLinkDefination $ ReferenceLinkDefinationData label dest title

instance HasMath InlineAst where
  inlineMath x = rawNode $ InlineMath $ InlineMathParams x
  displayMath x = rawNode $ DisplayMath $ DisplayMathParams x

instance Commonmark.Extensions.HasEmoji InlineAst where
  emoji x y = rawNode $ Emoji $ EmojiParams x y

instance HasSubscript InlineAst where
  subscript = rawNode . Subscript . SubscriptParams

instance HasSuperscript InlineAst where
  superscript = rawNode . Superscript . SuperscriptParams

instance HasStrikethrough InlineAst where
  strikethrough = rawNode . Strikethrough . StrikethroughParams

instance HasPipeTable InlineAst BlockAst where
  pipeTable align header rows = rawNode $ PipeTable $ PipeTableParams align header rows

instance HasDefinitionList InlineAst BlockAst where
  definitionList spacing xs = rawNode $ DefinitionList $ DefinitionListParams spacing xs

instance HasTaskList InlineAst BlockAst where
  taskList listtype spacing items = rawNode $ TaskList $ TaskListParams listtype spacing items

instance HasAlerts InlineAst BlockAst where
  alert a = rawNode . Alert . AlertParams a

instance HasWikilinks InlineAst where
  wikilink target inline = rawNode $ WikiLink $ WikiLinkParams target inline

instance HasFootnote InlineAst BlockAst where
  footnote num label bl = rawNode $ Footnote $ FootnoteParams num label bl
  footnoteList items = rawNode $ FootnoteList $ FootnoteListParams items
  footnoteRef num label bl = rawNode $ FootnoteRef $ FootnoteRefParams num label bl

instance HasQuoted InlineAst where
  singleQuoted = rawNode . SingleQuoted . SingleQuotedParams
  doubleQuoted = rawNode . DoubleQuoted . DoubleQuotedParams

instance HasSpan InlineAst where
  spanWith _ [] = []
  spanWith attrs1 [AstNode item sr attrs2] = [AstNode item sr (attrs1 ++ attrs2)]
  spanWith attrs1 xs = [AstNode (Span (SpanParams xs)) Nothing attrs1]

instance HasDiv (GenericAst k) where
  div_ = id

class HasPlainTextBuilder x where
  toPlainTextBuilder :: x -> TB.Builder

instance HasPlainTextBuilder (MdNode k) where
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
    Div (DivParams asts) -> mconcat $ map toPlainTextBuilder asts

instance HasPlainTextBuilder (GenericAst k) where
  toPlainTextBuilder = foldr ((<>) . toPlainTextBuilder) ""

instance ToPlainText (GenericAst k) where
  toPlainText = LT.toStrict . TB.toLazyText . toPlainTextBuilder

instance HasPlaceholder InlineAst where
  placeholder = rawNode . Placeholder . PlaceholderParams

children :: MarkdownElement k -> [GenericAst k]
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
children _ = []

inlines :: MarkdownElement Bl -> [InlineAst]
inlines (Paragraph params) = [params ^. inline]
inlines (Plain params) = [params ^. inline]
inlines (Header params) = [params ^. inline]
inlines (PipeTable params) = concat (params ^. headers : params ^. rows)
inlines (DefinitionList params) =
  map fst (params ^. defListItems)
    ++ ((params ^. defListItems) >>= snd >>= map (^. paramaters) >>= inlines)
inlines bl = children bl >>= map (^. paramaters) >>= inlines

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

class HasAllNodes (k1 :: MarkdownAstKind) (k2 :: MarkdownAstKind) where
  allNodes' :: (MdNode k1 -> Maybe a) -> [MdNode k2] -> [a]
  allNodes :: (MdNode k1 -> Bool) -> [MdNode k2] -> [MdNode k1]

instance HasAllNodes k k where
  allNodes' :: (MdNode k -> Maybe a) -> GenericAst k -> [a]
  allNodes' f = concatMap (helper f)
    where
      helper f node =
        case f node of
          Just x -> [x]
          Nothing -> concatMap (allNodes' f) $ children $ node ^. paramaters
  allNodes :: (MdNode k -> Bool) -> GenericAst k -> [MdNode k]
  allNodes f = concatMap (helper f)
    where
      helper f node = if f node then [node] else concatMap (allNodes f) (children $ node ^. paramaters)

instance HasAllNodes Il Bl where
  allNodes' :: (MdNode Il -> Maybe a) -> GenericAst Bl -> [a]
  allNodes' f ast = map (^. paramaters) ast >>= inlines >>= allNodes' f
  allNodes :: (MdNode Il -> Bool) -> GenericAst Bl -> [MdNode Il]
  allNodes f ast = map (^. paramaters) ast >>= inlines >>= allNodes f

firstNode :: (MdNode k -> Bool) -> GenericAst k -> Maybe (MdNode k)
firstNode f ast = listToMaybe (allNodes f ast)

firstNode' :: (MdNode k -> Maybe a) -> GenericAst k -> Maybe a
firstNode' f = listToMaybe . allNodes' f

nodeAt :: (Integral i) => (MdNode k -> Bool) -> i -> i -> GenericAst k -> Maybe (MdNode k)
nodeAt f row col =
  firstNode \node -> case node ^. sourceRange of
    Just sr -> inRange row col sr && f node
    _ -> False

nodeAt' :: (Integral i) => (MdNode k -> Maybe a) -> i -> i -> GenericAst k -> Maybe a
nodeAt' f row col = firstNode' \node -> do
  sr <- node ^. sourceRange
  guard $ inRange row col sr
  f node

findPlaceholders :: MarkdownAst -> [AstNode PlaceholderParams]
findPlaceholders = (allNodes' :: (MdNode Il -> Maybe a) -> BlockAst -> [a]) \case
  (AstNode (Placeholder params) sr attr) -> Just (AstNode params sr attr)
  _ -> Nothing

findLinks :: MarkdownAst -> [AstNode (LinkParams InlineAst)]
findLinks = (allNodes' :: (MdNode Il -> Maybe a) -> BlockAst -> [a]) \case
  (AstNode (Link params) sr attr) -> Just (AstNode params sr attr)
  _ -> Nothing

findTasks :: MarkdownAst -> [AstNode (TaskListParams InlineAst BlockAst)]
findTasks = (allNodes' :: (MdNode Bl -> Maybe a) -> BlockAst -> [a]) \case
  (AstNode (TaskList params) sr attr) -> Just (AstNode params sr attr)
  _ -> Nothing

findAlerts :: MarkdownAst -> [AstNode (AlertParams InlineAst BlockAst)]
findAlerts = (allNodes' :: (MdNode Bl -> Maybe a) -> BlockAst -> [a]) \case
  (AstNode (Alert params) sr attr) -> Just (AstNode params sr attr)
  _ -> Nothing

findHaders :: MarkdownAst -> [AstNode (HeaderParams InlineAst)]
findHaders = (allNodes' :: (MdNode Bl -> Maybe a) -> BlockAst -> [a]) \case
  (AstNode (Header params) sr attr) -> Just (AstNode params sr attr)
  _ -> Nothing
