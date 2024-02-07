{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( entrance,
  )
where

import Cli.NewNoteGen (LookupTable, fromConfig, replacePlaceholders)
import Data.Aeson (Value (String), decode)
import Data.Char (isSpace)
import Data.HashMap.Internal.Array (write)
import Data.Maybe (fromJust, fromMaybe)
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (formatTime)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.LocalTime
import qualified Data.Yaml as Y
import Model.Config
import Model.Document (Document (metadata))
import Model.Metadata
import Options.Applicative
import Parser.Markdown (allSpecExtensions, extensionLookup)
import Parser.Opts
import System.FilePath
import Text.RawString.QQ

initNotebook :: InitOptions -> IO ()
initNotebook _ = do
  let exampleConfig =
        [r|{
  "template":{
    "name": "John",
    "email": "john@example.com",
    "dateFormat": "%Y-%m-%d",
    "timeFormat": "%H:%M:%S",
    "dateTimeFormat": "%Y-%m-%dT%H:%M:%S",
    "variables": {
      "game": "League of Legends"
    }
  },
  "extensions": ["math", "emoji", "footnotes"]
}|]
  writeFile "config.json" exampleConfig
  let exampleTemplate =
        [r|# {{title}}

Today is {{date}} and it's {{time}} now.
I'm writing about {{game}}.
|]
  writeFile "default.md" exampleTemplate

replaceTime :: LocalTime -> T.Text -> LookupTable -> LookupTable
replaceTime tim s tab = (s, T.pack $ formatTime defaultTimeLocale (T.unpack $ fromJust (lookup s tab)) tim) : tab

newNote :: NewOptions -> IO ()
newNote op = do
  conf <- readFile "config.json"
  let config =
        case decode $ fromString conf of
          Just x -> x
          Nothing -> error "config: Decode failed"
  let tab = table op ++ fromConfig config
  tim <- getCurrentTime
  tz <- getTimeZone tim
  let localtim = utcToLocalTime tz tim
  let newtab = foldr (replaceTime localtim) tab ["date", "time", "dateTime"]
  let filename = joinPath [dir op, "default.md"]
  template <- readFile filename
  let replacedContent = replacePlaceholders (foldMap (\x -> fromMaybe mempty $ lookup (T.unpack x) extensionLookup) (extensions config)) filename (T.pack template) newtab
  let mdata =
        Metadata
          { title = Just $ Parser.Opts.title op,
            author = lookup "name" newtab,
            dateTime = Just tim,
            tags = [],
            description = ""
          }
  let output = joinPath [dir op, T.unpack (T.map (\c -> if isSpace c then '-' else c) $ T.toLower $ Parser.Opts.title op) ++ ".md"]
  writeFile output ("---\n" ++ T.unpack (T.decodeUtf8 (Y.encode mdata)) ++ "---\n" ++ T.unpack replacedContent)
  putStrLn output

entrance :: Options -> IO ()
entrance op = case optCommand op of
  Init x -> initNotebook x
  New y -> newNote y
