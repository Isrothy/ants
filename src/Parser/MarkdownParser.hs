module Parser.MarkdownParser
  ( inlineElements,
    inlineElement,
    text,
    strong,
    emphasis,
    link,
    image,
    highlight,
    strikeThrough,
    inlineCode,
    footnoteRef,
  )
where

import Data.Char
import Data.Functor
import Parser.Main
import Parser.MarkdownSyntax

cattext :: [InlineElement] -> [InlineElement]
cattext = foldr go []
  where
    go (Text a) (Text b : xs) = Text (a ++ b) : xs
    go x xs = x : xs

isLineSeparator :: Char -> Bool
isLineSeparator c = c `elem` ['\n', '\r', '\x0085', '\x2028', '\x2029']

noLineSeparator :: Parser Char
noLineSeparator = satisfy $ not . isLineSeparator

manyInlineTextTill :: Parser end -> Parser [Char]
manyInlineTextTill = manyTill noLineSeparator

-- Constants for inline element markers
linkStart :: String
linkStart = "["

imageStart :: String
imageStart = "!"

strongDelimiter :: String
strongDelimiter = "**"

emphasisDelimiter :: String
emphasisDelimiter = "*"

highlightDelimiter :: String
highlightDelimiter = "=="

strikethroughDelimiter :: String
strikethroughDelimiter = "~~"

inlineCodeDelimiter :: String
inlineCodeDelimiter = "`"

footnoteRefStart :: String
footnoteRefStart = "[^"

-- Parser for Text Element
text :: Parser InlineElement
text = do
  parsedText <-
    manyInlineTextTill $
      lookAhead $
        choice $
          map
            string
            [ linkStart,
              imageStart ++ linkStart,
              strongDelimiter,
              emphasisDelimiter,
              highlightDelimiter,
              strikethroughDelimiter,
              inlineCodeDelimiter,
              footnoteRefStart
            ]
  return $ Text parsedText

-- Parser for Footnote Reference
footnoteRef :: Parser InlineElement
footnoteRef = do
  _ <- string footnoteRefStart
  refText <- manyTill (satisfy $ not . isSpace) (char ']')
  return $ FootnoteRef (FootnoteReference refText)

-- Parsing links, in the format [text](url "title")
link :: Parser InlineElement
link = do
  _ <- string linkStart
  linkText <- manyTill inlineElement (string "]")
  _ <- char '('
  url <- manyInlineTextTill (choice [char ' ', char ')'])
  title <- optional (between (char '"') (char '"') (manyInlineTextTill (char '"')))
  _ <- char ')'
  return $ Link (LinkData url title) (cattext linkText)

-- Parsing images, in the format ![altText](url "title")
image :: Parser InlineElement
image = do
  _ <- string (imageStart ++ linkStart)
  altText <- manyInlineTextTill (char ']')
  _ <- char '('
  url <- manyInlineTextTill (choice [char ' ', char ')'])
  title <- optional (between (char '"') (char '"') (manyInlineTextTill (char '"')))
  _ <- char ')'
  return $ Image (ImageData url altText title)

-- Parsing strong text, in the format **strong text**
strong :: Parser InlineElement
strong = do
  _ <- string strongDelimiter
  strongText <- manyTill inlineElement (string strongDelimiter)
  return $ Strong (cattext strongText)

-- Parsing emphasis text, in the format *emphasis text*
emphasis :: Parser InlineElement
emphasis = do
  _ <- string emphasisDelimiter
  italicText <- manyTill inlineElement (string emphasisDelimiter)
  return $ Emphasis (cattext italicText)

-- Parsing highlighted text, in the format ==highlighted text==
highlight :: Parser InlineElement
highlight = do
  _ <- string highlightDelimiter
  emphasisText <- manyTill inlineElement (string highlightDelimiter)
  return $ Emphasis (cattext emphasisText)

-- Parsing strikethrough text, in the format ~~strikethrough text~~
strikeThrough :: Parser InlineElement
strikeThrough = do
  _ <- string strikethroughDelimiter
  strikeThroughText <- manyTill inlineElement (string strikethroughDelimiter)
  return $ Strikethrough (cattext strikeThroughText)

-- Parsing inline code, in the format `inline code`
inlineCode :: Parser InlineElement
inlineCode = do
  _ <- string inlineCodeDelimiter
  codeText <- manyInlineTextTill (string inlineCodeDelimiter)
  return $ InlineCode codeText

-- Parser for Inline Elements
inlineElements :: Parser [InlineElement]
inlineElements = many inlineElement <&> cattext

inlineElement :: Parser InlineElement
inlineElement =
  choice
    [ text,
      strong,
      emphasis,
      link,
      image,
      highlight,
      strikeThrough,
      inlineCode,
      footnoteRef
    ]
