{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.MarkdownAst
  ( MarkdownAst (..),
    MarkdownElement (..),
    children,
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

data MarkdownAst where
  MarkdownAst ::
    { markdownElement :: MarkdownElement,
      sourceRange :: Maybe SourceRange,
      attributes :: Attributes
    } ->
    MarkdownAst
  deriving (Show, Eq)

rawNode :: MarkdownElement -> MarkdownAst
rawNode x = MarkdownAst x Nothing []

data MarkdownElement where
  Text :: T.Text -> MarkdownElement
  Entity :: T.Text -> MarkdownElement
  LineBreak :: MarkdownElement
  SoftBreak :: MarkdownElement
  EscapedChar :: Char -> MarkdownElement
  Code :: T.Text -> MarkdownElement
  Emphasis :: MarkdownAst -> MarkdownElement
  Strong :: MarkdownAst -> MarkdownElement
  Link :: T.Text -> T.Text -> (Maybe MarkdownAst) -> MarkdownElement
  Image :: T.Text -> T.Text -> (Maybe MarkdownAst) -> MarkdownElement
  Strikethrough :: MarkdownAst -> MarkdownElement
  Highlight :: MarkdownAst -> MarkdownElement
  RawInline :: Format -> T.Text -> MarkdownElement
  Alert :: AlertType -> (Maybe MarkdownAst) -> MarkdownElement
  Emoji :: T.Text -> T.Text -> MarkdownElement
  InlineMath :: T.Text -> MarkdownElement
  DisplayMath :: T.Text -> MarkdownElement
  SingleQuoted :: (Maybe MarkdownAst) -> MarkdownElement
  DoubleQuoted :: (Maybe MarkdownAst) -> MarkdownElement
  Subscript :: MarkdownAst -> MarkdownElement
  Superscript :: MarkdownAst -> MarkdownElement
  Paragraph :: MarkdownAst -> MarkdownElement
  Plain :: MarkdownAst -> MarkdownElement
  Header :: Int -> (Maybe MarkdownAst) -> MarkdownElement
  List ::
    ListType ->
    ListSpacing ->
    [Maybe MarkdownAst] ->
    MarkdownElement
  Blockquote :: MarkdownAst -> MarkdownElement
  CodeBlock :: T.Text -> T.Text -> MarkdownElement
  RawBlock :: Format -> T.Text -> MarkdownElement
  HorizontalRule :: MarkdownElement
  PipeTable ::
    [ColAlignment] ->
    [Maybe MarkdownAst] ->
    [[Maybe MarkdownAst]] ->
    MarkdownElement
  ReferenceLinkDefination :: T.Text -> (T.Text, T.Text) -> MarkdownElement
  DefinitionList ::
    ListSpacing ->
    [(Maybe MarkdownAst, [Maybe MarkdownAst])] ->
    MarkdownElement
  TaskList ::
    ListType ->
    ListSpacing ->
    [(Bool, Maybe MarkdownAst)] ->
    MarkdownElement
  WikiLink :: T.Text -> MarkdownAst -> MarkdownElement
  Footnote :: Int -> T.Text -> (Maybe MarkdownAst) -> MarkdownElement
  FootnoteList :: [Maybe MarkdownAst] -> MarkdownElement
  FootnoteRef :: T.Text -> T.Text -> MarkdownAst -> MarkdownElement
  Placeholder :: T.Text -> MarkdownElement
  Span :: [MarkdownAst] -> MarkdownElement
  deriving (Show, Eq)

instance Semigroup MarkdownAst where
  (MarkdownAst (Span xs) Nothing []) <> (MarkdownAst (Span ys) Nothing []) =
    rawNode . Span $ xs ++ ys
  (MarkdownAst (Span xs) Nothing []) <> y = rawNode . Span $ xs ++ [y]
  x <> (MarkdownAst (Span ys) Nothing []) = rawNode . Span $ x : ys
  x <> y = rawNode . Span $ [x, y]

instance HasAttributes (Maybe MarkdownAst) where
  addAttributes _ Nothing = Nothing
  addAttributes attrs1 (Just (MarkdownAst item sr attrs2)) = Just (MarkdownAst item sr (attrs1 ++ attrs2))

instance Rangeable (Maybe MarkdownAst) where
  ranged _ Nothing = Nothing
  ranged sr (Just (MarkdownAst item _ attrs)) = Just (MarkdownAst item (Just sr) attrs)

instance IsInline (Maybe MarkdownAst) where
  lineBreak = Just $ rawNode LineBreak
  softBreak = Just $ rawNode SoftBreak
  entity = Just . rawNode . Entity
  emph = fmap $ rawNode . Emphasis
  strong = fmap $ rawNode . Strong
  link target title inline = Just $ rawNode $ Link target title inline
  image source title inline = Just $ rawNode $ Image source title inline
  code x = Just $ rawNode $ Code x
  rawInline format x = Just $ rawNode $ RawInline format x
  str = Just . rawNode . Text
  escapedChar = Just . rawNode . EscapedChar

instance IsBlock (Maybe MarkdownAst) (Maybe MarkdownAst) where
  paragraph = fmap $ rawNode . Paragraph
  plain = fmap $ rawNode . Plain
  thematicBreak = Just $ rawNode HorizontalRule
  blockQuote = fmap $ rawNode . Blockquote
  codeBlock info t = Just $ rawNode $ CodeBlock info t
  heading level il = Just $ rawNode $ Header level il
  rawBlock format t = Just $ rawNode $ RawBlock format t
  list listtype spacing items = Just $ rawNode $ List listtype spacing items
  referenceLinkDefinition label (dest, title) = Just $ rawNode (ReferenceLinkDefination label (dest, title))

instance HasMath (Maybe MarkdownAst) where
  inlineMath x = Just $ rawNode $ InlineMath x
  displayMath x = Just $ rawNode $ DisplayMath x

instance HasEmoji (Maybe MarkdownAst) where
  emoji x y = Just $ rawNode $ Emoji x y

instance HasSubscript (Maybe MarkdownAst) where
  subscript = fmap $ rawNode . Subscript

instance HasSuperscript (Maybe MarkdownAst) where
  superscript = fmap $ rawNode . Superscript

instance HasStrikethrough (Maybe MarkdownAst) where
  strikethrough = fmap $ rawNode . Strikethrough

instance HasPipeTable (Maybe MarkdownAst) (Maybe MarkdownAst) where
  pipeTable align header rows = Just $ rawNode $ PipeTable align header rows

instance HasDefinitionList (Maybe MarkdownAst) (Maybe MarkdownAst) where
  definitionList spacing xs = Just $ rawNode $ DefinitionList spacing xs

instance HasTaskList (Maybe MarkdownAst) (Maybe MarkdownAst) where
  taskList listtype spacing items = Just $ rawNode (TaskList listtype spacing items)

instance HasAlerts (Maybe MarkdownAst) (Maybe MarkdownAst) where
  alert a = Just . rawNode . Alert a

instance HasWikilinks (Maybe MarkdownAst) where
  wikilink target = fmap $ rawNode . WikiLink target

instance HasFootnote (Maybe MarkdownAst) (Maybe MarkdownAst) where
  footnote num label bl = Just $ rawNode $ Footnote num label bl
  footnoteList items = Just $ rawNode $ FootnoteList items
  footnoteRef num label = fmap $ rawNode . FootnoteRef num label

instance HasQuoted (Maybe MarkdownAst) where
  singleQuoted = Just . rawNode . SingleQuoted
  doubleQuoted = Just . rawNode . DoubleQuoted

instance HasSpan (Maybe MarkdownAst) where
  spanWith _ Nothing = Nothing
  spanWith attrs1 (Just (MarkdownAst item sr attrs2)) = Just (MarkdownAst item sr (attrs1 ++ attrs2))

instance HasDiv (Maybe MarkdownAst) where
  div_ = id

toPlainTextBuilder' :: MarkdownAst -> TB.Builder
toPlainTextBuilder' (MarkdownAst ele _ _) = case ele of
  Text t -> TB.fromText t
  Entity t -> TB.fromText t
  LineBreak -> "\n"
  SoftBreak -> " "
  EscapedChar c -> TB.singleton c
  Code t -> TB.fromText t
  Emphasis ast -> toPlainTextBuilder' ast
  Strong ast -> toPlainTextBuilder' ast
  Link _ _ ast -> toPlainTextBuilder ast
  Image _ _ ast -> toPlainTextBuilder ast
  Strikethrough ast -> toPlainTextBuilder' ast
  Highlight ast -> toPlainTextBuilder' ast
  RawInline _ t -> TB.fromText t
  Emoji _ t -> TB.fromText t
  InlineMath t -> TB.fromText t
  DisplayMath t -> TB.fromText t
  SingleQuoted ast -> "\'" <> toPlainTextBuilder ast <> "\'"
  DoubleQuoted ast -> "\"" <> toPlainTextBuilder ast <> "\""
  Subscript ast -> " " <> toPlainTextBuilder' ast <> " "
  Superscript ast -> " " <> toPlainTextBuilder' ast <> " "
  Paragraph ast -> toPlainTextBuilder' ast <> "\n"
  Plain ast -> toPlainTextBuilder' ast
  Alert a ast -> TB.fromText (alertName a) <> ": " <> toPlainTextBuilder ast
  Header _ ast -> toPlainTextBuilder ast <> "\n"
  List _ _ asts -> mconcat $ map (\ast -> toPlainTextBuilder ast <> "\n") asts
  Blockquote ast -> toPlainTextBuilder' ast <> "\n"
  CodeBlock _ t -> TB.fromText t <> "\n"
  RawBlock _ t -> TB.fromText t <> "\n"
  HorizontalRule -> "\n\n"
  PipeTable _ title rows -> tableHelper (title : rows) <> "\n"
    where
      tableHelper :: [[Maybe MarkdownAst]] -> TB.Builder
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
  WikiLink txt ast -> TB.fromText txt <> toPlainTextBuilder' ast
  Footnote num txt ast ->
    "[" <> TB.fromString (show num) <> "] " <> TB.fromText txt <> toPlainTextBuilder ast
  FootnoteList items -> mconcat $ map (\item -> toPlainTextBuilder item <> "\n") items
  FootnoteRef num _ _ -> "[" <> TB.fromString (show num) <> "] "
  Placeholder _ -> ""
  Span asts -> mconcat $ map toPlainTextBuilder' asts

toPlainTextBuilder :: Maybe MarkdownAst -> TB.Builder
toPlainTextBuilder Nothing = ""
toPlainTextBuilder (Just ast) = toPlainTextBuilder' ast

instance ToPlainText (Maybe MarkdownAst) where
  toPlainText = LT.toStrict . TB.toLazyText . toPlainTextBuilder

instance HasPlaceholder (Maybe MarkdownAst) where
  placeholder = Just . rawNode . Placeholder

children :: MarkdownElement -> [MarkdownAst]
children (Emphasis ast) = [ast]
children (Strong ast) = [ast]
children (Strikethrough ast) = [ast]
children (Superscript ast) = [ast]
children (Subscript ast) = [ast]
children (Link _ _ ast) = maybeToList ast
children (Image _ _ ast) = maybeToList ast
children (Span asts) = asts
children (Paragraph ast) = [ast]
children (Plain ast) = [ast]
children (Header _ ast) = maybeToList ast
children (List _ _ asts) = catMaybes asts
children (Blockquote ast) = [ast]
children (PipeTable _ title rows) = catMaybes (concat (title : rows))
children (DefinitionList _ asts) = mapMaybe fst asts ++ concatMap (catMaybes . snd) asts
children (TaskList _ _ items) = mapMaybe snd items
children (WikiLink _ ast) = [ast]
children (Footnote _ _ ast) = maybeToList ast
children (FootnoteList items) = catMaybes items
children (FootnoteRef _ _ ast) = [ast]
children (Alert _ ast) = maybeToList ast
children _ = []

findPlaceholders :: MarkdownAst -> [(T.Text, SourceRange)]
findPlaceholders (MarkdownAst (Placeholder txt) (Just srange) _) = [(txt, srange)]
findPlaceholders (MarkdownAst (Placeholder _) _ _) = []
findPlaceholders (MarkdownAst ele _ _) = concatMap findPlaceholders (children ele)

findLinks :: MarkdownAst -> [(T.Text, T.Text)]
findLinks (MarkdownAst (Link target title _) _ _) = [(target, title)]
findLinks (MarkdownAst ele _ _) = concatMap findLinks (children ele)

findTasks :: MarkdownAst -> [(Bool, Maybe MarkdownAst)]
findTasks (MarkdownAst (TaskList _ _ items) _ _) = items
findTasks (MarkdownAst ele _ _) = concatMap findTasks (children ele)

findFinishedTasks :: MarkdownAst -> [Maybe MarkdownAst]
findFinishedTasks = map snd . filter fst . findTasks

findUnfinishedTasks :: MarkdownAst -> [Maybe MarkdownAst]
findUnfinishedTasks = map snd . filter (not . fst) . findTasks

findAlerts :: MarkdownAst -> [(AlertType, Maybe MarkdownAst)]
findAlerts (MarkdownAst (Alert t ast) _ _) = [(t, ast)]
findAlerts (MarkdownAst ele _ _) = concatMap findAlerts (children ele)
