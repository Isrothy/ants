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
import Data.HashMap.Internal.Array (write)
import Data.Maybe (fromJust)
import Data.String (fromString)
import qualified Data.Text as T
import Data.Time (formatTime)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale)
import Data.Time.LocalTime
import Model.Config
import Options.Applicative
import Parser.Opts
import System.FilePath
import Text.RawString.QQ
import Parser.Markdown (allSpecExtensions)
import Model.Document (Document(metadata))
import Model.Metadata
import qualified Data.Yaml as Y
import qualified Data.Text.Encoding as T
import Data.Char (isSpace)

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
  }
}|]
  writeFile "config.json" exampleConfig
  let exampleTemplate =
        [r|# {title}
Today is {date} and it's {time} now.
I'm writing about {game}.
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
  let replacedContent = replacePlaceholders mempty filename (T.pack template) newtab
  let mdata = Metadata {
    title = Just $ Parser.Opts.title op,
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
