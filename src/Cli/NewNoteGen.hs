{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Cli.NewNoteGen
  ( splitLines,
    replacePlaceholders,
    fromConfig,
    findPosition,
    newNote,
  )
where

import Commonmark
import Control.Monad
import Control.Monad.Extra (fromMaybeM)
import qualified Data.Bifunctor
import Data.Char (isSpace)
import Data.Functor.Identity
import Data.List (sortBy)
import Data.Maybe (fromJust, fromMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.Time
import qualified Data.Yaml as Y
import Model.Config (getSyntaxSpec)
import qualified Model.Config as Config
import Model.MarkdownAst
import qualified Model.Metadata as M
import Parser.Markdown
import Parser.Opts
import Parser.Placeholder
import Path (parseRelFile, toFilePath, (</>))
import Path.IO (resolveDir')
import Project.ProjectRoot (defaultTemplateFileName, findRoot, findTemplate, readConfig, templateDir)
import Safe

markdownAstWithPlaceholder ::
  (Monad m) =>
  SyntaxSpec m MarkdownAst MarkdownAst ->
  String ->
  T.Text ->
  m (Either Commonmark.ParseError MarkdownAst)
markdownAstWithPlaceholder exts =
  markdownAstWith (placeholderSpec <> exts <> defaultSyntaxSpec)

replacePlaceholders ::
  SyntaxSpec Identity MarkdownAst MarkdownAst ->
  T.Text ->
  [(T.Text, T.Text)] ->
  T.Text
replacePlaceholders exts text lookupTable =
  case runIdentity $ markdownAstWithPlaceholder exts "" text of
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
    fromTemplate tl =
      maybeToList (("name",) <$> Config.name tl)
        <> maybeToList (("email",) <$> Config.email tl)
        <> [("date", fromMaybe "%Y-%m-%d" $ Config.dateFormat tl)]
        <> [("time", fromMaybe "%H:%M:%S" $ Config.timeFormat tl)]
        <> [("dateTime", fromMaybe "%Y-%m-%dT%H:%M:%S" $ Config.dateTimeFormat tl)]
        <> Config.variables tl

replaceTime :: LocalTime -> T.Text -> LookupTable -> LookupTable
replaceTime tim s tab = (s, T.pack $ formatTime defaultTimeLocale (T.unpack $ fromJust (lookup s tab)) tim) : tab

newNote :: NewOptions -> IO ()
newNote op = do
  pathToRoot <- fromMaybeM (error "Cannot find config") findRoot
  config <- fromMaybeM (error "config: Decode failed") $ readConfig pathToRoot
  let tab = table op ++ fromConfig config
  tim <- getCurrentTime
  tz <- getTimeZone tim
  let localtim = utcToLocalTime tz tim
  let newtab = foldr (replaceTime localtim) tab ["date", "time", "dateTime"]
  actualPath <- resolveDir' $ dir op
  pathToTemplate <- findTemplate actualPath
  let templatePath = fmap (</> templateDir </> defaultTemplateFileName) pathToTemplate
  templateString <- maybe (pure "") (readFile . toFilePath) templatePath
  let replacedContent = replacePlaceholders (getSyntaxSpec config) (T.pack templateString) newtab
  let mdata =
        M.Metadata
          { M.title = Just $ Parser.Opts.title op,
            M.author = lookup "name" newtab,
            M.dateTime = Just tim,
            M.tags = [],
            M.description = ""
          }
  filename <- parseRelFile (T.unpack (T.map (\c -> if isSpace c then '-' else c) $ T.toLower $ Parser.Opts.title op) ++ ".md")
  let output = actualPath </> filename
  writeFile (toFilePath output) ("---\n" ++ T.unpack (T.decodeUtf8 (Y.encode mdata)) ++ "---\n" ++ T.unpack replacedContent)
  putStrLn $ toFilePath output
