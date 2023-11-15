module Metadata
  ( Metadata (..),
  )
where

data Metadata = Metadata
  { title :: String,
    author :: String,
    date :: String,
    tags :: [String],
    description :: String,
    uid :: Int
  }
  deriving (Show, Eq)
