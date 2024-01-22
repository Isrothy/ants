module Util.Fuzzy
  ( editDistance,
    minEditDistanceSubstring,
    matches,
    contains,
  )
where

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

matches :: (Eq a) => [a] -> [a] -> Bool
matches xs ys = editDistance xs ys <= floor (log (fromIntegral (min (length xs) (length ys)) :: Double))

contains :: (Eq a) => [a] -> [a] -> Bool
contains xs ys = minEditDistanceSubstring xs ys <= floor (log (fromIntegral (length xs) :: Double))
