{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cli.InitNoteBook
  ( initNotebook,
  )
where

import Parser.Opts
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
