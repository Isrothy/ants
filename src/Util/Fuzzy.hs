{-# LANGUAGE FlexibleContexts #-}

module Util.Fuzzy
  ( editDistance,
    minEditDistanceSubstring,
    matches,
    isInfixOf,
    matchesT,
    isInfixOfT,
  )
where

import Control.Loop
import Control.Monad.ST (runST)
import qualified Data.Text as T
import Data.Text.Internal.Fusion hiding (length)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

textToVector :: T.Text -> UV.Vector Char
textToVector t =
  case stream t of
    Stream step s0 _ -> UV.create $ do
      m <- MUV.new (T.length t)
      let go s i = case step s of
            Done -> pure ()
            Skip s' -> go s' i
            Yield x s' -> do
              MUV.write m i x
              go s' (i + 1)
      go s0 0
      pure m

dp :: (GV.Vector v a, Eq a) => Bool -> v a -> v a -> UV.Vector Int
dp substrings xs ys = runST $ do
  let (m, n) = (GV.length xs, GV.length ys)
  curRow <- MUV.new (n + 1)
  newRow <- MUV.new (n + 1)

  forLoop 0 (<= n) (+ 1) $ \j -> do
    let val = if substrings then 0 else j
    MUV.write curRow j val

  forLoop 1 (<= m) (+ 1) $ \i -> do
    MUV.write newRow 0 i
    forLoop 1 (<= n) (+ 1) $ \j -> do
      diag <- MUV.read curRow (j - 1)
      left <- MUV.read newRow (j - 1)
      up <- MUV.read curRow j
      let xChar = xs GV.! (i - 1)
          yChar = ys GV.! (j - 1)
      MUV.write newRow j $ if xChar == yChar then diag else 1 + min3 left up diag
    MUV.copy curRow newRow

  UV.freeze curRow
  where
    min3 x y z = min x (min y z)

editDistanceV :: (GV.Vector v a, Eq a) => v a -> v a -> Int
editDistanceV xs ys = UV.last $ dp False xs ys

minEditDistanceSubstringV :: (GV.Vector v a, Eq a) => v a -> v a -> Int
minEditDistanceSubstringV xs ys = GV.minimum $ dp True xs ys

editDistance :: (Eq a) => [a] -> [a] -> Int
editDistance xs ys = editDistanceV (V.fromList xs) (V.fromList ys)

minEditDistanceSubstring :: (Eq a) => [a] -> [a] -> Int
minEditDistanceSubstring xs ys = minEditDistanceSubstringV (V.fromList xs) (V.fromList ys)

threshold :: Int -> Int
threshold x = (x + 2) `div` 5 -- a magic number. May exist better threshold

matchesV :: (GV.Vector v a, Eq a) => v a -> v a -> Bool
matchesV xs ys = editDistanceV xs ys <= threshold (min (GV.length xs) (GV.length ys))

isInfixOfV :: (GV.Vector v a, Eq a) => v a -> v a -> Bool
isInfixOfV xs ys = minEditDistanceSubstringV xs ys <= threshold (GV.length xs)

matches :: (Eq a) => [a] -> [a] -> Bool
matches xs ys = matchesV (V.fromList xs) (V.fromList ys)

isInfixOf :: (Eq a) => [a] -> [a] -> Bool
isInfixOf xs ys = isInfixOfV (V.fromList xs) (V.fromList ys)

preProcess :: T.Text -> UV.Vector Char
preProcess = textToVector . T.toCaseFold . T.strip

matchesT :: T.Text -> T.Text -> Bool
matchesT xs ys = editDistanceV xs' ys' <= threshold (max (UV.length xs') (UV.length ys'))
  where
    xs' = preProcess xs
    ys' = preProcess ys

isInfixOfT :: T.Text -> T.Text -> Bool
isInfixOfT xs ys = minEditDistanceSubstringV xs' ys' <= threshold (UV.length xs')
  where
    xs' = preProcess xs
    ys' = preProcess ys
