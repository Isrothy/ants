{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Model.MarkdownAst
  ( MarkdownAst,
    MarkdownElement (..),
    findPlaceholders,
    findLinks,
    findTasks,
    findFinishedTasks,
    findUnfinishedTasks,
    findAlerts,
  )
where

import Commonmark
import Commonmark.Extensions
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Internal.Builder as TB
import qualified Data.Text.Lazy as LT
import Parser.Placeholder

data Node = Node
  { element :: MarkdownElement,
    sourceRange :: Maybe SourceRange,
    attributes :: Attributes
  }
  deriving (Show, Eq)

type MarkdownAst = [Node]

rawNode :: MarkdownElement -> MarkdownAst
rawNode x = [Node x Nothing []]

data MarkdownElement where
  Text :: T.Text -> MarkdownElement
  Entity :: T.Text -> MarkdownElement
  LineBreak :: MarkdownElement
  SoftBreak :: MarkdownElement
  EscapedChar :: Char -> MarkdownElement
  Code :: T.Text -> MarkdownElement
  Emphasis :: MarkdownAst -> MarkdownElement
  Strong :: MarkdownAst -> MarkdownElement
  Link :: T.Text -> T.Text -> MarkdownAst -> MarkdownElement
  Image :: T.Text -> T.Text -> MarkdownAst -> MarkdownElement
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
  List ::
    ListType ->
    ListSpacing ->
    [MarkdownAst] ->
    MarkdownElement
  Blockquote :: MarkdownAst -> MarkdownElement
  CodeBlock :: T.Text -> T.Text -> MarkdownElement
  RawBlock :: Format -> T.Text -> MarkdownElement
  HorizontalRule :: MarkdownElement
  PipeTable ::
    [ColAlignment] ->
    [MarkdownAst] ->
    [[MarkdownAst]] ->
    MarkdownElement
  ReferenceLinkDefination :: T.Text -> (T.Text, T.Text) -> MarkdownElement
  DefinitionList ::
    ListSpacing ->
    [(MarkdownAst, [MarkdownAst])] ->
    MarkdownElement
  TaskList ::
    ListType ->
    ListSpacing ->
    [(Bool, MarkdownAst)] ->
    MarkdownElement
  WikiLink :: T.Text -> MarkdownAst -> MarkdownElement
  Footnote :: Int -> T.Text -> MarkdownAst -> MarkdownElement
  FootnoteList :: [MarkdownAst] -> MarkdownElement
  FootnoteRef :: T.Text -> T.Text -> MarkdownAst -> MarkdownElement
  Placeholder :: T.Text -> MarkdownElement
  Span :: MarkdownAst -> MarkdownElement
  deriving (Show, Eq)

instance HasAttributes MarkdownAst where
  addAttributes _ [] = []
  addAttributes attrs1 [Node item sr attrs2] = [Node item sr (attrs1 ++ attrs2)]
  addAttributes attrs1 xs = [Node (Span xs) Nothing attrs1]

instance Rangeable MarkdownAst where
  ranged _ [] = []
  ranged sr [Node item _ attrs] = [Node item (Just sr) attrs]
  ranged sr xs = [Node (Span xs) (Just sr) []]

instance IsInline MarkdownAst where
  lineBreak = rawNode LineBreak
  softBreak = rawNode SoftBreak
  entity = rawNode . Entity
  emph = rawNode . Emphasis
  strong = rawNode . Strong
  link target title inline = rawNode $ Link target title inline
  image source title inline = rawNode $ Image source title inline
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
  list listtype spacing items = rawNode $ List listtype spacing items
  referenceLinkDefinition label (dest, title) = rawNode (ReferenceLinkDefination label (dest, title))

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
  wikilink target = rawNode . WikiLink target

instance HasFootnote MarkdownAst MarkdownAst where
  footnote num label bl = rawNode $ Footnote num label bl
  footnoteList items = rawNode $ FootnoteList items
  footnoteRef num label = rawNode . FootnoteRef num label

instance HasQuoted MarkdownAst where
  singleQuoted = rawNode . SingleQuoted
  doubleQuoted = rawNode . DoubleQuoted

instance HasSpan MarkdownAst where
  spanWith _ [] = []
  spanWith attrs1 [Node item sr attrs2] = [Node item sr (attrs1 ++ attrs2)]
  spanWith attrs1 xs = [Node (Span xs) Nothing attrs1]

instance HasDiv MarkdownAst where
  div_ = id

toPlainTextBuilder' :: Node -> TB.Builder
toPlainTextBuilder' (Node ele _ _) = case ele of
  Text t -> TB.fromText t
  Entity t -> TB.fromText t
  LineBreak -> "\n"
  SoftBreak -> " "
  EscapedChar c -> TB.singleton c
  Code t -> TB.fromText t
  Emphasis ast -> toPlainTextBuilder ast
  Strong ast -> toPlainTextBuilder ast
  Link _ _ ast -> toPlainTextBuilder ast
  Image _ _ ast -> toPlainTextBuilder ast
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
  List _ _ asts -> mconcat $ map (\ast -> toPlainTextBuilder ast <> "\n") asts
  Blockquote ast -> toPlainTextBuilder ast <> "\n"
  CodeBlock _ t -> TB.fromText t <> "\n"
  RawBlock _ t -> TB.fromText t <> "\n"
  HorizontalRule -> "\n\n"
  PipeTable _ title rows -> tableHelper (title : rows) <> "\n"
    where
      tableHelper [] = ""
      tableHelper (x : xs) =
        mconcat (map (\y -> toPlainTextBuilder y <> " ") x) <> "\n" <> tableHelper xs
  ReferenceLinkDefination label (dest, title) ->
    "[" <> TB.fromText label <> "] " <> TB.fromText dest <> " " <> TB.fromText title
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
  WikiLink txt ast -> TB.fromText txt <> toPlainTextBuilder ast
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
children (Link _ _ ast) = [ast]
children (Image _ _ ast) = [ast]
children (Span asts) = [asts]
children (Paragraph ast) = [ast]
children (Plain ast) = [ast]
children (Header _ ast) = [ast]
children (List _ _ asts) = asts
children (Blockquote ast) = [ast]
children (PipeTable _ title rows) = concat (title : rows)
children (DefinitionList _ asts) = map fst asts ++ concatMap snd asts
children (TaskList _ _ items) = map snd items
children (WikiLink _ ast) = [ast]
children (Footnote _ _ ast) = [ast]
children (FootnoteList items) = items
children (FootnoteRef _ _ ast) = [ast]
children (Alert _ ast) = [ast]
children _ = []

findPlaceholders :: MarkdownAst -> [(T.Text, SourceRange)]
findPlaceholders = concatMap \case
  (Node (Placeholder txt) srange _) -> maybeToList (fmap (txt,) srange)
  (Node ele _ _) -> concatMap findPlaceholders (children ele)

findLinks :: MarkdownAst -> [(T.Text, T.Text)]
findLinks = concatMap \case
  (Node (Link target title _) _ _) -> [(target, title)]
  (Node ele _ _) -> concatMap findLinks (children ele)

findTasks :: MarkdownAst -> [(Bool, MarkdownAst)]
findTasks = concatMap \case
  (Node (TaskList _ _ items) _ _) -> items
  (Node ele _ _) -> concatMap findTasks (children ele)

findFinishedTasks :: MarkdownAst -> [MarkdownAst]
findFinishedTasks = map snd . filter fst . findTasks

findUnfinishedTasks :: MarkdownAst -> [MarkdownAst]
findUnfinishedTasks = map snd . filter (not . fst) . findTasks

findAlerts :: MarkdownAst -> [(AlertType, MarkdownAst)]
findAlerts = concatMap \case
  (Node (Alert t ast) _ _) -> [(t, ast)]
  (Node ele _ _) -> concatMap findAlerts (children ele)
