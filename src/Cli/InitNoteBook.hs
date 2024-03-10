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
import Path (toFilePath, (</>))
import Path.IO (createDirIfMissing)
import Project.ProjectRoot (configDir, configFileName, defaultTemplateFileName, templateDir)
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
  "extensions": ["gfm"]
}|]
  createDirIfMissing True configDir
  writeFile (toFilePath $ configDir </> configFileName) exampleConfig
  let exampleTemplate =
        [r|# {{title}}

Today is {{date}} and it's {{time}} now.
I'm writing about {{game}}.
|]
  createDirIfMissing True templateDir
  writeFile (toFilePath $ templateDir </> defaultTemplateFileName) exampleTemplate
