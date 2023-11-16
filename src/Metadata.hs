{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Metadata
  ( Metadata (..),
  )
where

import Common.Default
import Common.ExAeson
import Data.Aeson
import qualified Data.Text as T

data Metadata = Metadata
  { title :: !(Maybe T.Text),
    author :: !(Maybe T.Text),
    date :: !(Maybe T.Text),
    tags :: ![T.Text],
    description :: !T.Text
  }
  deriving (Show, Eq)

instance Default Metadata where
  def = Metadata Nothing Nothing Nothing [] ""

instance FromJSON Metadata where
  parseJSON = withObject "metadata" $ \o ->
    Metadata
      <$> (o .:?? "title")
      <*> (o .:?? "author")
      <*> (o .:?? "date")
      <*> ( o .:?? "tags" >>= parseList \case
              String v -> Just v
              _ -> Nothing
          )
      <*> (o .:?? "description" .!= "")
