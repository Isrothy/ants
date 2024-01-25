{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Benchmarks.Util.Fuzzy
import Criterion.Main

main :: IO ()
main = do
  Benchmarks.Util.Fuzzy.benchmark
