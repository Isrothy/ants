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

import Control.Monad.Extra
import Model.Config (getSyntaxSpec)
import Model.DocQuery (query)
import Model.Document (Document (relPath, metadata))
import Parser.DocQuery (completeQuery)
import Parser.Opts (FilterOptions (..))
import Path.IO (getCurrentDir)
import Path.Posix (toFilePath)
import Project.DocLoader (loadAllFromDirectory)
import Project.ProjectRoot (findRoot, readConfig)
import Text.Parsec (parse)
import Text.Parsec.Error (errorMessages, messageString)
import Data.List (sortBy)
import qualified Data.Text as T
import Model.Metadata (Metadata(..))
comparebyField :: Maybe T.Text -> Document -> Document -> Ordering
comparebyField x a b = case x of
  Just "title" -> compare (title $ metadata a) (title $ metadata b)
  Just "author" -> compare (author $ metadata a) (author $ metadata b)
  Just "time" -> compare (dateTime $ metadata a) (dateTime $ metadata b)
  _ -> compare (relPath a) (relPath b)
filterNotes :: FilterOptions -> IO ()
filterNotes op = do
  let qs = case queryString op of
        "" -> "content:/.*/"
        x -> x
  let q = parse completeQuery "" $ qs
  let expr = either (error . concatMap messageString . errorMessages) id q
  pathToRoot <- fromMaybeM (error "Cannot find config") findRoot
  config <- fromMaybeM (error "config: Decode failed") $ readConfig pathToRoot
  cwd <- getCurrentDir
  docs <- loadAllFromDirectory (getSyntaxSpec config) cwd
  let filt = query expr
  let filtered = filter filt docs
  let res = sortBy (comparebyField $ sortString op) filtered
  mapM_ (putStrLn . toFilePath . relPath) res
