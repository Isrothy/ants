-- Main Markdown Document
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Parser.MarkdownSyntax where

import Parser.Main

{-# HLINT ignore "Use newtype instead of data" #-}
newtype MarkdownDocument = MarkdownDocument [BlockElement]

-- Block Elements
data BlockElement
  = Paragraph [InlineElement]
  | Header Int (Maybe String) [InlineElement]
  | UnorderedList [ListItem]
  | OrderedList [ListItem]
  | Blockquote [BlockElement]
  | CodeBlock String
  | HorizontalRule
  | TableElement Table
  | DefinitionListElement DefinitionList
  | TaskList [TaskItem]
  | FootnoteDef FootnoteDefinition

-- Inline Elements
data InlineElement
  = Text String
  | Emphasis [InlineElement]
  | Strong [InlineElement]
  | InlineCode String
  | Link LinkData [InlineElement]
  | Image ImageData
  | Strikethrough [InlineElement]
  | Highlight [InlineElement]
  | FootnoteRef FootnoteReference

-- Link and Image Metadata
data LinkData = LinkData
  { linkURL :: String,
    linkTitle :: Maybe String
  }

data ImageData = ImageData
  { imageURL :: String,
    imageAlt :: String,
    imageTitle :: Maybe String
  }

-- List Item
newtype ListItem = ListItem [BlockElement]

-- Table
data Table = Table
  { tableHeader :: TableRow,
    tableRows :: [TableRow]
  }

newtype TableRow = TableRow [TableCell]

newtype TableCell = TableCell [InlineElement]

-- Footnotes
newtype FootnoteReference = FootnoteReference String

data FootnoteDefinition = FootnoteDefinition String [BlockElement]

-- Definition Lists
newtype DefinitionList = DefinitionList [(Term, [Definition])]

newtype Term = Term [InlineElement]

newtype Definition = Definition [BlockElement]

-- Task Lists
data TaskItem = TaskItem Bool [BlockElement]

