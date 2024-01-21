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
    toPlainText',
  )
where

import Commonmark
import Commonmark.Extensions
import Data.Maybe
import Data.Text (Text, pack)
import Parser.Placeholder

data MarkdownAst = MarkdownAst
  { markdownElement :: !MarkdownElement,
    sourceRange :: !(Maybe SourceRange),
    attributes :: !Attributes
  }
  deriving (Show, Eq)

rawNode :: MarkdownElement -> MarkdownAst
rawNode x = MarkdownAst x Nothing []

data MarkdownElement where
  Text :: Text -> MarkdownElement
  Entity :: Text -> MarkdownElement
  LineBreak :: MarkdownElement
  SoftBreak :: MarkdownElement
  EscapedChar :: Char -> MarkdownElement
  Code :: Text -> MarkdownElement
  Emphasis :: MarkdownAst -> MarkdownElement
  Strong :: MarkdownAst -> MarkdownElement
  Link :: Text -> Text -> (Maybe MarkdownAst) -> MarkdownElement
  Image :: Text -> Text -> (Maybe MarkdownAst) -> MarkdownElement
  Strikethrough :: MarkdownAst -> MarkdownElement
  Highlight :: MarkdownAst -> MarkdownElement
  RawInline :: Format -> Text -> MarkdownElement
  Alert :: AlertType -> (Maybe MarkdownAst) -> MarkdownElement
  Emoji :: Text -> Text -> MarkdownElement
  InlineMath :: Text -> MarkdownElement
  DisplayMath :: Text -> MarkdownElement
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
  CodeBlock :: Text -> Text -> MarkdownElement
  RawBlock :: Format -> Text -> MarkdownElement
  HorizontalRule :: MarkdownElement
  PipeTable ::
    [ColAlignment] ->
    [Maybe MarkdownAst] ->
    [[Maybe MarkdownAst]] ->
    MarkdownElement
  ReferenceLinkDefination :: Text -> (Text, Text) -> MarkdownElement
  DefinitionList ::
    ListSpacing ->
    [(Maybe MarkdownAst, [Maybe MarkdownAst])] ->
    MarkdownElement
  TaskList ::
    ListType ->
    ListSpacing ->
    [(Bool, Maybe MarkdownAst)] ->
    MarkdownElement
  WikiLink :: Text -> MarkdownAst -> MarkdownElement
  Footnote :: Int -> Text -> (Maybe MarkdownAst) -> MarkdownElement
  FootnoteList :: [Maybe MarkdownAst] -> MarkdownElement
  FootnoteRef :: Text -> Text -> MarkdownAst -> MarkdownElement
  Placeholder :: Text -> MarkdownElement
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

toPlainText' :: MarkdownAst -> Text
toPlainText' (MarkdownAst ele _ _) = case ele of
  Text t -> t
  Entity t -> t
  LineBreak -> "\n"
  SoftBreak -> " "
  EscapedChar c -> pack [c]
  Code t -> t
  Emphasis ast -> toPlainText' ast
  Strong ast -> toPlainText' ast
  Link _ _ ast -> toPlainText ast
  Image _ _ ast -> toPlainText ast
  Strikethrough ast -> toPlainText' ast
  Highlight ast -> toPlainText' ast
  RawInline _ t -> t
  Emoji _ t -> t
  InlineMath t -> t
  DisplayMath t -> t
  SingleQuoted ast -> "\'" <> toPlainText ast <> "\'"
  DoubleQuoted ast -> "\"" <> toPlainText ast <> "\""
  Subscript ast -> " " <> toPlainText' ast <> " "
  Superscript ast -> " " <> toPlainText' ast <> " "
  Paragraph ast -> toPlainText' ast <> "\n"
  Plain ast -> toPlainText' ast
  Alert a ast -> alertName a <> ": " <> toPlainText ast
  Header _ ast -> toPlainText ast <> "\n"
  List _ _ asts -> mconcat $ map (\ast -> toPlainText ast <> "\n") asts
  Blockquote ast -> toPlainText' ast <> "\n"
  CodeBlock _ t -> t <> "\n"
  RawBlock _ t -> t <> "\n"
  HorizontalRule -> "\n\n"
  PipeTable _ title rows -> tableHelper (title : rows) <> "\n"
    where
      tableHelper :: [[Maybe MarkdownAst]] -> Text
      tableHelper [] = ""
      tableHelper (x : xs) =
        mconcat (map (\y -> toPlainText y <> "\n") x) <> "\n" <> tableHelper xs
  ReferenceLinkDefination label (dest, title) -> "[" <> label <> "] " <> dest <> " " <> title
  DefinitionList _ asts ->
    mconcat
      ( map
          ( \(term, defs) ->
              toPlainText term <> "\n" <> mconcat (map (\def -> toPlainText def <> "\n") defs)
          )
          asts
      )
  TaskList _ _ items -> mconcat $ map (\(_, ast) -> toPlainText ast <> "\n") items
  WikiLink txt ast -> txt <> toPlainText' ast
  Footnote num txt ast -> "[" <> pack (show num) <> "] " <> txt <> toPlainText ast
  FootnoteList items -> mconcat $ map (\item -> toPlainText item <> "\n") items
  FootnoteRef num label _ -> "[" <> pack (show num) <> "] " <> label
  Placeholder _ -> ""
  Span asts -> mconcat $ map toPlainText' asts

instance ToPlainText (Maybe MarkdownAst) where
  toPlainText Nothing = ""
  toPlainText (Just ast) = toPlainText' ast

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

findPlaceholders :: MarkdownAst -> [(Text, SourceRange)]
findPlaceholders (MarkdownAst (Placeholder txt) (Just srange) _) = [(txt, srange)]
findPlaceholders (MarkdownAst (Placeholder _) _ _) = []
findPlaceholders (MarkdownAst ele _ _) = concatMap findPlaceholders (children ele)

findLinks :: MarkdownAst -> [(Text, Text)]
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
