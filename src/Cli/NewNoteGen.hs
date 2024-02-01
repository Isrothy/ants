{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Cli.NewNoteGen
  ( splitLines,
    replacePlaceholders,
    fromConfig,
    findPosition,
    LookupTable,
  )
where

import Commonmark
import Control.Monad
import qualified Data.Bifunctor
import Data.Functor.Identity
import Data.List (sortBy)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Model.Config as Config
import Model.MarkdownAst
import Parser.Markdown
import Parser.Placeholder
import Safe

type LookupTable = [(T.Text, T.Text)]

markdownAstWithPlaceholder ::
  (Monad m) =>
  SyntaxSpec m MarkdownAst MarkdownAst ->
  String ->
  T.Text ->
  m (Either ParseError MarkdownAst)
markdownAstWithPlaceholder extensions =
  markdownAstWith (placeholderSpec <> extensions <> defaultSyntaxSpec)

replacePlaceholders ::
  SyntaxSpec Identity MarkdownAst MarkdownAst ->
  String ->
  T.Text ->
  [(T.Text, T.Text)] ->
  T.Text
replacePlaceholders extensions filename text lookupTable =
  case runIdentity $ markdownAstWithPlaceholder extensions filename text of
    Left _ -> text
    Right ast ->
      replacePlaceholders'
        text
        (sortBy posReverse (findPlaceholders ast))
        (map (Data.Bifunctor.first T.toCaseFold) lookupTable)

posReverse :: (T.Text, SourceRange) -> (T.Text, SourceRange) -> Ordering
posReverse (_, SourceRange ((pos1, _) : _)) (_, SourceRange ((pos2, _) : _)) =
  pos2 `compare` pos1
posReverse _ _ = error "posReverse: empty source range"

-- For Text.lines does not treat '\r' (CR, carriage return) as a newline character.
splitLines :: T.Text -> [T.Text]
splitLines txt = map (T.reverse . TL.toStrict) $ splitLines' (TL.fromStrict txt) TL.empty

splitLines' :: TL.Text -> TL.Text -> [TL.Text]
splitLines' txt line = case TL.uncons txt of
  Nothing -> [line]
  Just ('\r', rest) -> case TL.uncons rest of
    Just ('\n', rest') -> ('\n' `TL.cons` '\r' `TL.cons` line) : splitLines' rest' TL.empty
    _ -> ('\r' `TL.cons` line) : splitLines' rest TL.empty
  Just ('\n', rest) -> ('\n' `TL.cons` line) : splitLines' rest TL.empty
  Just (c, rest) -> splitLines' rest (c `TL.cons` line)

findPosition :: Int -> Int -> T.Text -> Maybe Int
findPosition lineNumber columnNumber text = do
  guard $ lineNumber >= 1 && columnNumber >= 1
  let lines' = splitLines text
  guard $ lineNumber <= length lines'
  let line = at lines' (lineNumber - 1)
  guard $ columnNumber <= T.length line
  let linesBefore = take (lineNumber - 1) lines'
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
replacePlaceholders' origin ((ph, range) : rest) lookupTable =
  case lookup (T.toCaseFold ph) lookupTable of
    Nothing -> replacePlaceholders' origin rest lookupTable
    Just replacement ->
      replacePlaceholders'
        (replace origin (head $ unSourceRange range) replacement)
        rest
        lookupTable

fromConfig :: Config.Config -> LookupTable
fromConfig = fromTemplate . Config.template
  where
    fromTemplate :: Config.Template -> LookupTable
    fromTemplate template =
      maybeToList (("name",) <$> Config.name template)
        <> maybeToList (("email",) <$> Config.email template)
        <> [("date", fromMaybe "%Y-%m-%d" $ Config.dateFormat template)]
        <> [("time", fromMaybe "%H:%M:%S" $ Config.timeFormat template)]
        <> [("dateTime", fromMaybe "%Y-%m-%dT%H:%M:%S" $ Config.dateTimeFormat template)]
        <> Config.variables template
