{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parser.MarkdownAst
  ( MarkdownAst (..),
    markdownAst,
    markdownAstWith,
    MarkdownElement (..),
    allSpecExtions,
  )
where

import Commonmark
import Commonmark.Extensions
import Data.Foldable
import Data.Text (Text, pack)

markdownAst :: String -> Text -> Either ParseError (Maybe MarkdownAst)
markdownAst = commonmark

allSpecExtions :: SyntaxSpec (Either ParseError) (Maybe MarkdownAst) (Maybe MarkdownAst)
allSpecExtions =
  fold
    [ autolinkSpec,
      pipeTableSpec,
      hardLineBreaksSpec,
      strikethroughSpec,
      superscriptSpec,
      subscriptSpec,
      smartPunctuationSpec,
      mathSpec,
      emojiSpec,
      footnoteSpec,
      definitionListSpec,
      fancyListSpec,
      taskListSpec,
      attributesSpec,
      rawAttributeSpec,
      bracketedSpanSpec,
      fencedDivSpec,
      autoIdentifiersSpec,
      autoIdentifiersAsciiSpec,
      implicitHeadingReferencesSpec,
      wikilinksSpec TitleBeforePipe,
      wikilinksSpec TitleAfterPipe,
      rebaseRelativePathsSpec,
      gfmExtensions
    ]

markdownAstWith ::
  (Monad m) =>
  SyntaxSpec m (Maybe MarkdownAst) (Maybe MarkdownAst) ->
  String ->
  Text ->
  m (Either ParseError (Maybe MarkdownAst))
markdownAstWith = commonmarkWith

data MarkdownAst = MarkdownAst
  { markdownElement :: MarkdownElement,
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
  | SingleQuoted (Maybe MarkdownAst)
  | DoubleQuoted (Maybe MarkdownAst)
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

instance HasQuoted (Maybe MarkdownAst) where
  singleQuoted = justRawNode . SingleQuoted
  doubleQuoted = justRawNode . DoubleQuoted

instance HasSpan (Maybe MarkdownAst) where
  spanWith _ Nothing = Nothing
  spanWith attrs1 (Just (MarkdownAst item sr attrs2)) = Just (MarkdownAst item sr (attrs1 ++ attrs2))

instance HasDiv (Maybe MarkdownAst) where
  div_ = id

toPlainText1 :: MarkdownAst -> Text
toPlainText1 (MarkdownAst ele _ _) = case ele of
  Text t -> t
  Entity t -> t
  LineBreak -> pack "\n"
  SoftBreak -> pack " "
  EscapedChar c -> pack [c]
  Code t -> t
  Emphasis ast -> toPlainText1 ast
  Strong ast -> toPlainText1 ast
  Link _ _ ast -> toPlainText ast
  Image _ _ ast -> toPlainText ast
  Strikethrough ast -> toPlainText1 ast
  Highlight ast -> toPlainText1 ast
  RawInline _ t -> t
  Emoji _ t -> t
  InlineMath t -> t
  DisplayMath t -> t
  SingleQuoted ast -> toPlainText ast
  DoubleQuoted ast -> toPlainText ast
  Subscript ast -> toPlainText1 ast
  Superscript ast -> toPlainText1 ast
  Paragraph ast -> toPlainText1 ast
  Plain ast -> toPlainText1 ast
  Header _ ast -> toPlainText ast
  List _ _ asts -> mconcat $ map toPlainText asts
  Blockquote ast -> toPlainText1 ast
  CodeBlock _ t -> t
  RawBlock _ t -> t
  HorizontalRule -> pack "\n"
  PipeTable _ title rows -> helper (title : rows)
    where
      helper [] = pack ""
      helper (x : xs) = mconcat (map (\y -> toPlainText y <> pack " ") x) <> pack "\n" <> helper xs
  ReferenceLinkDefination label (dest, title) -> pack "[" <> label <> pack "] " <> dest <> pack " " <> title
  DefinitionList _ asts -> mconcat $ map toPlainText (map fst asts ++ concatMap snd asts)
  TaskList _ _ items -> mconcat $ map (toPlainText . snd) items
  WikiLink txt ast -> txt <> toPlainText1 ast
  Footnote num txt ast -> pack "[" <> pack (show num) <> pack "] " <> txt <> toPlainText ast
  FootnoteList items -> mconcat $ map toPlainText items
  FootnoteRef num label ast -> pack "[" <> pack (show num) <> pack "] " <> label <> toPlainText1 ast
  Span asts -> mconcat $ map toPlainText1 asts

instance ToPlainText (Maybe MarkdownAst) where
  toPlainText Nothing = pack ""
  toPlainText (Just ast) = toPlainText1 ast
