{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser.Opts
  ( Options (..),
    opts,
    Command (..),
    InitOptions (..),
    NewOptions (..),
    FilterOptions (..),
    LookupTable,
  )
where

import Data.List (elemIndex)
import Data.Maybe (maybeToList)
import Data.String (fromString)
import qualified Data.Text as T
import Options.Applicative

type LookupTable = [(T.Text, T.Text)]

data NewOptions = NewOptions
  { title :: T.Text,
    dir :: FilePath,
    table :: LookupTable
  }

data InitOptions = InitOptions
  {
  }

data FilterOptions = FilterOptions
  { queryString :: T.Text
  }

newtype Options = Options
  { optCommand :: Command
  }

data Command
  = New NewOptions
  | Init InitOptions
  | Filter FilterOptions

parsePair :: ReadM (T.Text, T.Text)
parsePair = eitherReader $ \s ->
  case elemIndex '=' s of
    Nothing -> Left "No = in variable assignment"
    Just idx -> Right (fromString (take idx s), fromString (drop (idx + 1) s))

newCommand :: Parser Command
newCommand = do
  title <- strOption (long "title" <> short 't' <> metavar "TITLE" <> help "title of note")
  name :: Maybe T.Text <-
    optional $
      strOption $
        long "name" <> short 'n' <> metavar "NAME" <> help "name of author"
  email :: Maybe T.Text <-
    optional $
      strOption $
        long "email" <> short 'e' <> metavar "admin@example.com" <> help "email of author"
  dateFormat :: Maybe T.Text <- optional $ strOption (long "dateFormat" <> metavar "FORMAT")
  timeFormat :: Maybe T.Text <- optional $ strOption (long "timeFormat" <> metavar "FORMAT")
  dateTimeFormat :: Maybe T.Text <- optional $ strOption (long "dateTimeFormat" <> metavar "FORMAT")
  dir <- strArgument (metavar "DIR" <> help "Directory to create the note" <> action "directory")
  vars :: LookupTable <-
    many $
      argument
        parsePair
        ( metavar "VARNAME=VALUE"
            <> help "Variables that take precedence over the ones in the config file"
        )
  pure $
    New $
      NewOptions
        { table =
            concatMap
              (\(a, b) -> zip [a] (maybeToList b))
              [ ("title", Just title),
                ("name", name),
                ("email", email),
                ("date", dateFormat),
                ("time", timeFormat),
                ("dateTime", dateTimeFormat)
              ]
              ++ vars,
          ..
        }

initCommand :: Parser Command
initCommand = do
  pure $ Init $ InitOptions {}

filterCommand :: Parser Command
filterCommand = do
  queryString <- strArgument (metavar "QUERY" <> help "Query to be used for filtering")
  pure $ Filter $ FilterOptions {..}

opts :: Parser Options
opts = do
  optCommand <-
    hsubparser
      ( command "init" (info initCommand $ progDesc "Init the notebook")
          <> command "new" (info newCommand $ progDesc "Create a new note")
          <> command "filter" (info filterCommand $ progDesc "Filter notes")
      )
  pure $ Options {..}
