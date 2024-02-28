{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Cli.FilterNotes
  ( filterNotes,
  )
where

import Model.DocQuery (query)
import Model.Document (Document (relPath))
import Parser.DocQuery (completeQuery)
import Parser.Markdown (getExtensionsFromConfig)
import Parser.Opts (FilterOptions (..))
import Path.IO (getCurrentDir)
import Path.Posix (toFilePath)
import Project.DocLoader (loadAllFromDirectory)
import Project.ProjectRoot (findRoot, readConfig)
import Text.Parsec (parse)
import Text.Parsec.Error (errorMessages, messageString)
import Control.Monad.Extra

filterNotes :: FilterOptions -> IO ()
filterNotes op = do
  let q = parse completeQuery "" $ queryString op
  let expr = either (error . concatMap messageString . errorMessages) id q
  pathToRoot <- fromMaybeM (error "Cannot find config") findRoot
  config <- fromMaybeM (error "config: Decode failed") $ readConfig pathToRoot
  cwd <- getCurrentDir
  docs <- loadAllFromDirectory (getExtensionsFromConfig config) cwd
  let filt = query expr
  let res = filter filt docs
  mapM_ (putStrLn . toFilePath . relPath) res
