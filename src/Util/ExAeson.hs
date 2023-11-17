module Util.ExAeson
  ( parseMap,
    parseList,
    parseString,
  )
where

import Data.Aeson
import Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bifunctor (second)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as Vector

parseMap :: (Value -> Maybe a) -> Value -> [(T.Text, a)]
parseMap f (Object obj) = [(toText k, v) | (k, Just v) <- map (second f) (KeyMap.toList obj)]
parseMap _ _ = []

parseList :: Value -> [T.Text]
parseList (Array arr) = mapMaybe parseString $ Vector.toList arr
parseList _ = []

parseString :: Value -> Maybe T.Text
parseString (String s) = Just s
parseString _ = Nothing
