{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Cli.ListNotes
  ( listNotes,
  )
where

import Control.Monad.Extra
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Model.Config (getSyntaxSpec)
import Model.DocQuery (query)
import Model.Document (Document (metadata, relPath))
import Model.Metadata (Metadata (..))
import Parser.DocQuery (completeQuery)
import Parser.Opts (ListOptions (..))
import Path.IO (getCurrentDir)
import Path.Posix (toFilePath)
import Project.DocLoader (loadAllFromDirectory)
import Project.ProjectRoot (findRoot, readConfig)
import Text.Parsec (parse)
import Text.Parsec.Error (errorMessages, messageString)

comparebyField :: Maybe T.Text -> Document -> Document -> Ordering
comparebyField x a b = case x of
  Just "title" -> compare (title $ metadata a) (title $ metadata b)
  Just "author" -> compare (author $ metadata a) (author $ metadata b)
  Just "time" -> compare (dateTime $ metadata a) (dateTime $ metadata b)
  _ -> compare (relPath a) (relPath b)

listNotes :: ListOptions -> IO ()
listNotes op = do
  pathToRoot <- fromMaybeM (error "Cannot find config") findRoot
  config <- fromMaybeM (error "config: Decode failed") $ readConfig pathToRoot
  cwd <- getCurrentDir
  docs <- loadAllFromDirectory (getSyntaxSpec config) cwd
  let qs = fromMaybe "content:/.*/" $ filterString op
  let q = parse completeQuery "" qs
  let expr = either (error . concatMap messageString . errorMessages) id q
  let filt = query expr
  filtered <- filterM filt docs
  let res = sortBy (comparebyField $ sortString op) filtered
  mapM_ (putStrLn . toFilePath . relPath) res
