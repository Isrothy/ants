{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.Aeson (decode)
import qualified Data.Bifunctor
import Data.Char (isSpace)
import Data.Functor.Identity
import Data.List (sortBy)
import Data.Maybe (fromJust, fromMaybe, maybeToList)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Data.Time
import qualified Data.Yaml as Y
import Model.Config
import qualified Model.Config as Config
import Model.MarkdownAst
import qualified Model.Metadata as M
import Parser.Markdown
import Parser.Opts
import Parser.Placeholder
import Path (mkRelFile, parseRelFile, toFilePath, (</>))
import Path.IO (resolveDir')
import Project.ProjectRoot (configDir, configFileName, defaultTemplateFileName, findRoot, findTemplate, templateDir)
import Safe

markdownAstWithPlaceholder ::
  (Monad m) =>
  SyntaxSpec m MarkdownAst MarkdownAst ->
  String ->
  T.Text ->
  m (Either Commonmark.ParseError MarkdownAst)
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

replaceTime :: LocalTime -> T.Text -> LookupTable -> LookupTable
replaceTime tim s tab = (s, T.pack $ formatTime defaultTimeLocale (T.unpack $ fromJust (lookup s tab)) tim) : tab

newNote :: NewOptions -> IO ()
newNote op = do
  pathToRoot <- findRoot
  let configPath =
        case pathToRoot of
          Just x -> x </> configDir </> configFileName
          Nothing -> error "Cannot find config"
  conf <- readFile $ toFilePath configPath
  let config =
        case Data.Aeson.decode $ fromString conf of
          Just x -> x
          Nothing -> error "config: Decode failed"
  let tab = table op ++ fromConfig config
  print tab
  tim <- getCurrentTime
  tz <- getTimeZone tim
  let localtim = utcToLocalTime tz tim
  let newtab = foldr (replaceTime localtim) tab ["date", "time", "dateTime"]
  actualPath <- resolveDir' $ dir op
  pathToTemplate <- findTemplate actualPath
  let templatePath =
        case pathToTemplate of
          Just x -> Just (x </> templateDir </> defaultTemplateFileName)
          Nothing -> Nothing
  templateString <- case templatePath of
    Just x -> readFile $ toFilePath x
    Nothing -> pure ""
  let replacedContent =
        case templatePath of
          Just fn -> replacePlaceholders (foldMap (\x -> fromMaybe mempty $ lookup (T.unpack x) extensionLookup) (extensions config)) (toFilePath fn) (T.pack templateString) newtab
          Nothing -> ""
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
