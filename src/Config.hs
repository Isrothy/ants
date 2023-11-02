{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Config
  ( Config (..),
    Template (..),
  )
where

import Data.Aeson
import Data.Text
import GHC.Generics

data Config where
  Config :: {template :: !(Maybe Template)} -> Config
  deriving (Show, Generic)

instance FromJSON Config

data Template where
  Template ::
    { name :: !(Maybe Text),
      email :: !(Maybe Text),
      dateFormat :: !(Maybe Text),
      timeFormat :: !(Maybe Text),
      dateTimeFormat :: !(Maybe Text),
      variables :: !(Maybe [(Text, Text)])
    } ->
    Template
  deriving (Show, Generic)

instance FromJSON Template
