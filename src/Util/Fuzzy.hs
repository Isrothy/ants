module Util.Fuzzy
  ( editDistance,
    minEditDistanceSubstring,
    matches,
    isInfixOf,
    matchesT,
    isInfixOfT,
  )
where

import qualified Data.Text as T
import GHC.Arr

dp :: (Eq a) => Bool -> [a] -> [a] -> [Int]
dp allSubstring xs ys = [table ! (m, i) | i <- [0 .. n]]
  where
    (m, n) = (length xs, length ys)
    x = array (1, m) (zip [1 ..] xs)
    y = array (1, n) (zip [1 ..] ys)

    table :: Array (Int, Int) Int
    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds = ((0, 0), (m, n))

    dist (0, j) = if allSubstring then 0 else j
    dist (i, 0) = i
    dist (i, j) =
      minimum
        [ table ! (i - 1, j) + 1,
          table ! (i, j - 1) + 1,
          if x ! i == y ! j then table ! (i - 1, j - 1) else 1 + table ! (i - 1, j - 1)
        ]

editDistance :: (Eq a) => [a] -> [a] -> Int
editDistance xs ys = last $ dp False xs ys

minEditDistanceSubstring :: (Eq a) => [a] -> [a] -> Int
minEditDistanceSubstring xs ys = minimum $ dp True xs ys

threshold :: Int -> Int
threshold x = floor $ sqrt (fromIntegral x :: Double)

matches :: (Eq a) => [a] -> [a] -> Bool
matches xs ys = editDistance xs ys <= threshold (min (length xs) (length ys))

isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf xs ys = minEditDistanceSubstring xs ys <= threshold (length xs)

preProcess :: T.Text -> String
preProcess = T.unpack . T.toCaseFold . T.strip

matchesT :: T.Text -> T.Text -> Bool
matchesT xs ys = editDistance xs' ys' <= threshold (min (length xs') (length ys'))
  where
    xs' = preProcess xs
    ys' = preProcess ys

isInfixOfT :: T.Text -> T.Text -> Bool
isInfixOfT xs ys = minEditDistanceSubstring xs' ys' <= threshold (length xs')
  where
    xs' = preProcess xs
    ys' = preProcess ys
