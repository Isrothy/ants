{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Metadata
  ( Metadata (..),
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Extra
import Data.Aeson.TH
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601

data Metadata = Metadata
  { title :: !(Maybe T.Text),
    author :: !(Maybe T.Text),
    dateTime :: !(Maybe UTCTime),
    tags :: ![T.Text],
    description :: !T.Text
  }
  deriving (Show, Eq)

instance Default Metadata where
  def = Metadata Nothing Nothing Nothing [] ""

instance FromJSON Metadata where
  parseJSON = withObject "metadata" $ \o ->
    Metadata
      <$> (o .:? "title" <|> pure Nothing)
      <*> (o .:? "author" <|> pure Nothing)
      <*> ((>>= iso8601ParseM) <$> o .:? "dateTime" <|> pure Nothing)
      <*> ((parseList . fromMaybe Null <$> (o .:? "tags")) <|> pure [])
      <*> (o .:? "description" .!= "" <|> pure "")

$(deriveToJSON defaultOptions {omitNothingFields = True} ''Metadata)
