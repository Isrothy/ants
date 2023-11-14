{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..),
    Template (..),
    Default (..),
  )
where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Key (fromString, fromText, toText)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Bifunctor (second)
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as Vector
import GHC.Generics

(.:??) :: (FromJSON a) => Object -> String -> Parser (Maybe a)
(.:??) o k = o .:? fromString k .!= Nothing <|> pure Nothing

parseMap :: (Value -> Maybe a) -> Maybe Value -> Parser [(T.Text, a)]
parseMap f (Just (Object obj)) = pure [(toText k, v) | (k, Just v) <- map (second f) (KeyMap.toList obj)]
parseMap _ _ = pure []

parseList :: (Value -> Maybe a) -> Maybe Value -> Parser [a]
parseList f (Just (Array arr)) = pure (mapMaybe f (Vector.toList arr))
parseList _ _ = pure []

class Default a where
  def :: a

instance Default Int where
  def = 0

instance Default [a] where
  def = []

instance Default (Maybe a) where
  def = Nothing

instance Default Bool where
  def = False

data Config where
  Config ::
    { template :: !Template,
      extensions :: ![T.Text]
    } ->
    Config
  deriving (Show, Generic, Eq)

instance FromJSON Config where
  parseJSON = withObject "config" $ \o ->
    Config
      <$> ((o .:?? "template") <&> fromMaybe def)
      <*> ( o .:?? "extensions" >>= parseList \case
              String v -> Just v
              _ -> Nothing
          )

instance ToJSON Config

instance Default Config where
  def = Config def []

data Template where
  Template ::
    { name :: !(Maybe T.Text),
      email :: !(Maybe T.Text),
      dateFormat :: !(Maybe T.Text),
      timeFormat :: !(Maybe T.Text),
      dateTimeFormat :: !(Maybe T.Text),
      variables :: ![(T.Text, T.Text)]
    } ->
    Template
  deriving (Show, Generic, Eq)

instance Default Template where
  def = Template Nothing Nothing Nothing Nothing Nothing []

instance FromJSON Template where
  parseJSON = withObject "template" $ \o ->
    Template
      <$> o .:?? "name"
      <*> o .:?? "email"
      <*> o .:?? "dateFormat"
      <*> o .:?? "timeFormat"
      <*> o .:?? "dateTimeFormat"
      <*> ( o .:?? "variables" >>= parseMap \case
              String v -> Just v
              _ -> Nothing
          )

variablesToJson :: [(T.Text, T.Text)] -> Value
variablesToJson vars = Array . Vector.fromList $ map (\(k, v) -> object [fromText k .= v]) vars

instance ToJSON Template where
  toJSON (Template name' email' dateFormat' timeFormat' dateTimeFormat' variables') =
    object $
      catMaybes
        [ (fromString "name" .=) <$> name',
          (fromString "email" .=) <$> email',
          (fromString "dateFormat" .=) <$> dateFormat',
          (fromString "timeFormat" .=) <$> timeFormat',
          (fromString "dateTimeFormat" .=) <$> dateTimeFormat',
          Just (fromString "variables" .= variablesToJson variables')
        ]
