module Common.ExAeson
  ( (.:??),
    parseMap,
    parseList,
  )
where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Key (fromString, toText)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Bifunctor (second)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as Vector

(.:??) :: (FromJSON a) => Object -> String -> Parser (Maybe a)
(.:??) o k = o .:? fromString k .!= Nothing <|> pure Nothing

parseMap :: (Value -> Maybe a) -> Maybe Value -> Parser [(T.Text, a)]
parseMap f (Just (Object obj)) = pure [(toText k, v) | (k, Just v) <- map (second f) (KeyMap.toList obj)]
parseMap _ _ = pure []

parseList :: (Value -> Maybe a) -> Maybe Value -> Parser [a]
parseList f (Just (Array arr)) = pure $ mapMaybe f $ Vector.toList arr
parseList _ _ = pure []
