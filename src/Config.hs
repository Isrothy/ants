{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..),
    Template (..),
  )
where

import Data.Aeson
import Data.Aeson.Key (fromString, fromText, toText)
import Data.Aeson.KeyMap (toList)
import Data.Aeson.Types (Parser)
import Data.Maybe
import qualified Data.Text as T
import Data.Vector (fromList)
import GHC.Generics

data Config where
  Config :: {template :: !(Maybe Template)} -> Config
  deriving (Show, Generic, Eq)

instance FromJSON Config

instance ToJSON Config

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

parseVariables :: Maybe Value -> Parser [(T.Text, T.Text)]
parseVariables (Just (Object obj)) = pure [(toText k, v) | (k, String v) <- toList obj]
parseVariables _ = pure []

instance FromJSON Template where
  parseJSON = withObject "template" $ \o ->
    Template
      <$> o .:? fromString "name"
      <*> o .:? fromString "email"
      <*> o .:? fromString "dateFormat"
      <*> o .:? fromString "timeFormat"
      <*> o .:? fromString "dateTimeFormat"
      <*> (o .:? fromString "variables" >>= parseVariables)

variablesToJson :: [(T.Text, T.Text)] -> Value
variablesToJson vars = Array . fromList $ map (\(k, v) -> object [fromText k .= v]) vars

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
