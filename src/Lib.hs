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
import Parser.Opts

entrance :: Options -> IO ()
entrance op = case optCommand op of
  Init x -> initNotebook x
  New y -> newNote y
