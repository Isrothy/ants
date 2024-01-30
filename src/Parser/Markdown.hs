module Parser.Markdown
  ( markdownAst,
    extensionLookup,
    allSpecExtensions,
    markdownAstWith,
  )
where

import Commonmark
import Commonmark.Extensions
import Data.Data
import qualified Data.Text as T
import Model.MarkdownAst

markdownAst :: String -> T.Text -> Either ParseError (Maybe MarkdownAst)
markdownAst = commonmark

extensionLookup ::
  ( Monad m,
    Typeable m,
    Typeable bl,
    Typeable il,
    HasPipeTable il bl,
    HasMath il,
    HasQuoted il,
    HasEmoji il,
    HasWikilinks il,
    HasSpan il,
    HasAlerts il bl,
    ToPlainText il,
    HasStrikethrough il,
    HasSuperscript il,
    HasSubscript il,
    HasDefinitionList il bl,
    HasDiv bl,
    HasTaskList il bl,
    HasFootnote il bl
  ) =>
  [(String, SyntaxSpec m il bl)]
extensionLookup =
  [ ("autolinks", autolinkSpec),
    ("pipe_tables", pipeTableSpec),
    ("hard_line_breaks", hardLineBreaksSpec),
    ("strikethrough", strikethroughSpec),
    ("superscript", superscriptSpec),
    ("subscript", subscriptSpec),
    ("smart", smartPunctuationSpec),
    ("math", mathSpec),
    ("emoji", emojiSpec),
    ("footnotes", footnoteSpec),
    ("definition_lists", definitionListSpec),
    ("fancy_lists", fancyListSpec),
    ("task_lists", taskListSpec),
    ("attributes", attributesSpec),
    ("raw_attribute", rawAttributeSpec),
    ("bracketed_spans", bracketedSpanSpec),
    ("fenced_divs", fencedDivSpec),
    ("auto_identifiers", autoIdentifiersSpec),
    ("auto_identifiers_ascii", autoIdentifiersAsciiSpec),
    ("implicit_heading_references", implicitHeadingReferencesSpec),
    ("wikilinks_title_before_pipe", wikilinksSpec TitleBeforePipe),
    ("wikilinks_title_after_pipe", wikilinksSpec TitleAfterPipe),
    ("rebase_relative_paths", rebaseRelativePathsSpec),
    ("alerts", alertSpec),
    ("gfm", gfmExtensions)
  ]

allSpecExtensions :: SyntaxSpec (Either ParseError) (Maybe MarkdownAst) (Maybe MarkdownAst)
allSpecExtensions = mconcat (map snd extensionLookup)

markdownAstWith ::
  (Monad m) =>
  SyntaxSpec m (Maybe MarkdownAst) (Maybe MarkdownAst) ->
  String ->
  T.Text ->
  m (Either ParseError (Maybe MarkdownAst))
markdownAstWith = commonmarkWith
