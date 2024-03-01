{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Benchmarks.Util.Fuzzy
  ( benchmark,
  )
where

import Criterion.Main
import qualified Data.Text as T
import System.Random (newStdGen, randomRs)
import Util.Fuzzy (isInfixOfT, minEditDistanceSubstring)

-- Generate a random string of given length
randomString :: Int -> IO String
randomString len = do
  take len . randomRs ('a', 'z') <$> newStdGen

benchmark :: IO ()
benchmark = do
  randomStr1 <- randomString 100
  randomStr2 <- randomString 100000
  randomText1 <- T.pack <$> randomString 100
  randomText2 <- T.pack <$> randomString 100000

  defaultMain
    [ bgroup
        "editDistance"
        [ bench "short strings" $ nf (uncurry minEditDistanceSubstring) ("kitten", "sitting"),
          bench "medium strings" $
            nf
              (uncurry minEditDistanceSubstring)
              (replicate 100 'a', replicate 1000 'b'),
          bench "long strings" $
            nf
              (uncurry minEditDistanceSubstring)
              (replicate 100 'a', replicate 10000 'b'),
          bench "complex random strings" $
            nf (uncurry minEditDistanceSubstring) (randomStr1, randomStr2)
        ],
      bgroup
        "isInfixOfT"
        [ bench "short texts" $ nf (uncurry isInfixOfT) (T.pack "hello", T.pack "hello world"),
          bench "medium texts" $ nf (uncurry isInfixOfT) (T.replicate 100 "a", T.replicate 1000 "b"),
          bench "long texts" $ nf (uncurry isInfixOfT) (T.replicate 100 "a", T.replicate 10000 "b"),
          bench "complex random texts" $ nf (uncurry isInfixOfT) (randomText1, randomText2)
        ]
    ]
