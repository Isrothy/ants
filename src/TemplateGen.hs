module TemplateGen
  ( splitLines,
    replacePlaceholders,
    findPosition,
  )
where

import Commonmark
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Text as T
import Parser.MarkdownAst
import Parser.Placeholder
import Text.Parsec.Pos

data Placeholder = Str String | DateTime String

type LookupTable = [(T.Text, T.Text)]

markdownAstWithPlaceholder ::
  String ->
  T.Text ->
  Maybe MarkdownAst
markdownAstWithPlaceholder filename text = case markdownAstWith (placeholderSpec <> allSpecExtions <> defaultSyntaxSpec) filename text of
  Left _ -> Nothing
  Right (Left _) -> Nothing
  Right (Right ast) -> ast

replacePlaceholders :: String -> T.Text -> LookupTable -> T.Text
replacePlaceholders filename text lookupTable = case markdownAstWithPlaceholder filename text of
  Nothing -> text
  Just ast -> replacePlaceholders' text (sortBy posReverse (findPlaceholders ast)) lookupTable

posReverse :: (T.Text, SourceRange) -> (T.Text, SourceRange) -> Ordering
posReverse (_, SourceRange ((pos1, _) : _)) (_, SourceRange ((pos2, _) : _)) =
  pos2 `compare` pos1

splitLines :: T.Text -> [T.Text]
splitLines txt = splitLines' txt T.empty

splitLines' :: T.Text -> T.Text -> [T.Text]
splitLines' txt line = case T.uncons txt of
  Nothing -> [line]
  Just ('\r', rest) -> case T.uncons rest of
    Just ('\n', rest') -> line `T.append` T.pack "\r\n" : splitLines' rest' T.empty
    _ -> line `T.append` T.pack "\r" : splitLines' rest T.empty
  Just ('\n', rest) -> line `T.append` T.pack "\n" : splitLines' rest T.empty
  Just (c, rest) -> splitLines' rest (line `T.append` T.singleton c)

findPosition :: Int -> Int -> T.Text -> Maybe Int
findPosition lineNumber columnNumber text = do
  guard $ lineNumber >= 1 && columnNumber >= 1
  let lines = splitLines text
  guard $ lineNumber <= length lines
  let line = lines !! (lineNumber - 1)
  guard $ columnNumber <= T.length line
  let linesBefore = take (lineNumber - 1) lines
  let charsInLinesBefore = sum $ map T.length linesBefore
  return $ charsInLinesBefore + columnNumber - 1

replace :: T.Text -> (SourcePos, SourcePos) -> T.Text -> T.Text
replace origin (start, end) replacement =
  fromMaybe
    origin
    ( do
        start' <- findPosition (sourceLine start) (sourceColumn start) origin
        end' <- findPosition (sourceLine end) (sourceColumn end) origin
        guard $ start' < end'
        return $ T.take start' origin <> replacement <> T.drop end' origin
    )

replacePlaceholders' :: T.Text -> [(T.Text, SourceRange)] -> LookupTable -> T.Text
replacePlaceholders' origin [] _ = origin
replacePlaceholders' origin ((placeholder, range) : rest) lookupTable =
  case lookup placeholder lookupTable of
    Nothing -> replacePlaceholders' origin rest lookupTable
    Just replacement ->
      replacePlaceholders'
        (replace origin (head $ unSourceRange range) replacement)
        rest
        lookupTable

-- replacePlaceholders' origin [] lookupTable = origin
-- replacePlaceholders' origin ((placeholder, range) : rest) lookupTable = replacePlaceholders' (replacePlaceholder origin placeholder range lookupTable) rest lookupTable
