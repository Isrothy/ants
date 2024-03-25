{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Cli.Graph
  ( printGraph,
  )
where
import Parser.Opts
import Control.Monad.Extra (fromMaybeM)
import Project.ProjectRoot (readConfig, findRoot)
import Path.IO (getCurrentDir)
import Project.DocLoader (loadAllFromDirectory)
import Model.Config (getSyntaxSpec)
import Model.Document (Document)
import Data.Graph (Graph)
getGraph :: [Document] -> IO Graph
getGraph = undefined
printGraph :: GraphOptions -> IO ()
printGraph _ = do
  pathToRoot <- fromMaybeM (error "Cannot find config") findRoot
  config <- fromMaybeM (error "config: Decode failed") $ readConfig pathToRoot
  cwd <- getCurrentDir
  docs <- loadAllFromDirectory (getSyntaxSpec config) cwd

  return ()