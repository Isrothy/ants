{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Benchmarks.Util.Fuzzy
  ( benchmark,
  )
where

import Criterion.Main
import qualified Data.Text as T
import System.Random (newStdGen, randomRs)
import Util.Fuzzy (editDistance, isInfixOfT)

-- Generate a random string of given length
randomString :: Int -> IO String
randomString len = do
  take len . randomRs ('a', 'z') <$> newStdGen

benchmark :: IO ()
benchmark = do
  randomStr1 <- randomString 1000
  randomStr2 <- randomString 1000
  randomText1 <- T.pack <$> randomString 1000
  randomText2 <- T.pack <$> randomString 1000

  defaultMain
    [ bgroup
        "editDistance"
        [ bench "short strings" $ nf (uncurry editDistance) ("kitten", "sitting"),
          bench "medium strings" $ nf (uncurry editDistance) (replicate 1000 'a', replicate 1000 'b'),
          bench "long strings" $ nf (uncurry editDistance) (replicate 10000 'a', replicate 10000 'b'),
          bench "complex random strings" $ nf (uncurry editDistance) (randomStr1, randomStr2)
        ],
      bgroup
        "isInfixOfT"
        [ bench "short texts" $ nf (uncurry isInfixOfT) (T.pack "hello", T.pack "hello world"),
          bench "medium texts" $ nf (uncurry isInfixOfT) (T.replicate 1000 "a", T.replicate 1000 "b"),
          bench "long texts" $ nf (uncurry isInfixOfT) (T.replicate 10000 "a", T.replicate 10000 "b"),
          bench "complex random texts" $ nf (uncurry isInfixOfT) (randomText1, randomText2)
        ]
    ]
