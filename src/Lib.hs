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

import Cli.InitNoteBook
import Cli.NewNoteGen
import Cli.Graph
import Parser.Opts
import Cli.ListNotes (listNotes)

entrance :: Options -> IO ()
entrance op = case optCommand op of
  Init x -> initNotebook x
  New y -> newNote y
  Lst z -> listNotes z
  Grph t -> printGraph t
