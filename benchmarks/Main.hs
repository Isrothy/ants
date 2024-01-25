module Main where

import qualified Benchmarks.Util.Fuzzy

main :: IO ()
main = do
  Benchmarks.Util.Fuzzy.benchmark
