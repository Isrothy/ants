{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parser.MarkdownAst
  ( MarkdownAst (..),
    markdownAst,
    markdownAstWith,
    MarkdownElement (..),
  )
where

import Commonmark
import Commonmark.Extensions
import Data.Text

markdownAst :: String -> Text -> Either ParseError (Maybe MarkdownAst)
markdownAst = commonmark

markdownAstWith ::
  (Monad m) =>
  SyntaxSpec m (Maybe MarkdownAst) (Maybe MarkdownAst) ->
  String ->
  Text ->
  m (Either ParseError (Maybe MarkdownAst))
markdownAstWith = commonmarkWith

data MarkdownAst = MarkdownAst
  { markdownElenment :: MarkdownElement,
    sourceRange :: Maybe SourceRange,
    attributes :: Attributes
  }
  deriving (Show, Eq)

rawNode :: MarkdownElement -> MarkdownAst
rawNode x = MarkdownAst x Nothing []

justRawNode :: MarkdownElement -> Maybe MarkdownAst
justRawNode = Just . rawNode

data MarkdownElement
  = Text Text
  | Entity Text
  | LineBreak
  | SoftBreak
  | EscapedChar Char
  | Code Text
  | Emphasis MarkdownAst
  | Strong MarkdownAst
  | Link Text Text (Maybe MarkdownAst)
  | Image Text Text (Maybe MarkdownAst)
  | Strikethrough MarkdownAst
  | Highlight MarkdownAst
  | RawInline Format Text
  | Emoji Text Text
  | InlineMath Text
  | DisplayMath Text
  | Subscript MarkdownAst
  | Superscript MarkdownAst
  | Paragraph MarkdownAst
  | Plain MarkdownAst
  | Header Int (Maybe MarkdownAst)
  | List ListType ListSpacing [Maybe MarkdownAst]
  | Blockquote MarkdownAst
  | CodeBlock Text Text
  | RawBlock Format Text
  | HorizontalRule
  | PipeTable [ColAlignment] [Maybe MarkdownAst] [[Maybe MarkdownAst]]
  | ReferenceLinkDefination Text (Text, Text)
  | DefinitionList ListSpacing [(Maybe MarkdownAst, [Maybe MarkdownAst])]
  | TaskList ListType ListSpacing [(Bool, Maybe MarkdownAst)]
  | WikiLink Text MarkdownAst
  | Footnote Int Text (Maybe MarkdownAst)
  | FootnoteList [Maybe MarkdownAst]
  | FootnoteRef Text Text MarkdownAst
  | Span [MarkdownAst]
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
  lineBreak = justRawNode LineBreak
  softBreak = justRawNode SoftBreak
  entity = justRawNode . Entity
  emph = fmap $ rawNode . Emphasis
  strong = fmap $ rawNode . Strong
  link target title inline = justRawNode $ Link target title inline
  image source title inline = justRawNode $ Image source title inline
  code x = justRawNode $ Code x
  rawInline format x = justRawNode $ RawInline format x
  str = justRawNode . Text
  escapedChar = justRawNode . EscapedChar

instance IsBlock (Maybe MarkdownAst) (Maybe MarkdownAst) where
  paragraph = fmap $ rawNode . Paragraph
  plain = fmap $ rawNode . Plain
  thematicBreak = justRawNode HorizontalRule
  blockQuote = fmap $ rawNode . Blockquote
  codeBlock info t = justRawNode $ CodeBlock info t
  heading level il = justRawNode $ Header level il
  rawBlock format t = justRawNode $ RawBlock format t
  list listtype spacing items = justRawNode $ List listtype spacing items
  referenceLinkDefinition label (dest, title) = justRawNode (ReferenceLinkDefination label (dest, title))

instance HasMath (Maybe MarkdownAst) where
  inlineMath x = justRawNode $ InlineMath x
  displayMath x = justRawNode $ DisplayMath x

instance HasEmoji (Maybe MarkdownAst) where
  emoji x y = justRawNode $ Emoji x y

instance HasSubscript (Maybe MarkdownAst) where
  subscript = fmap $ rawNode . Subscript

instance HasSuperscript (Maybe MarkdownAst) where
  superscript = fmap $ rawNode . Superscript

instance HasStrikethrough (Maybe MarkdownAst) where
  strikethrough = fmap $ rawNode . Strikethrough

instance HasPipeTable (Maybe MarkdownAst) (Maybe MarkdownAst) where
  pipeTable align header rows = justRawNode $ PipeTable align header rows

instance HasDefinitionList (Maybe MarkdownAst) (Maybe MarkdownAst) where
  definitionList spacing xs = justRawNode $ DefinitionList spacing xs

instance HasTaskList (Maybe MarkdownAst) (Maybe MarkdownAst) where
  taskList listtype spacing items = justRawNode (TaskList listtype spacing items)

instance HasWikilinks (Maybe MarkdownAst) where
  wikilink target = fmap $ rawNode . WikiLink target

instance HasFootnote (Maybe MarkdownAst) (Maybe MarkdownAst) where
  footnote num label bl = justRawNode $ Footnote num label bl
  footnoteList items = justRawNode $ FootnoteList items
  footnoteRef num label = fmap $ rawNode . FootnoteRef num label
