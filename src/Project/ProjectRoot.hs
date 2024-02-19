{-# LANGUAGE TemplateHaskell #-}

module Project.ProjectRoot
  ( findRoot,
    findTemplate,
    configDir,
    templateDir,
    configFileName,
    defaultTemplateFileName,
    readConfig,
  )
where

import Control.Conditional
import qualified Data.Aeson
import Data.String (fromString)
import Model.Config (Config)
import Path
import Path.IO

configDir :: Path Rel Dir
configDir = $(mkRelDir ".ants")

configFileName :: Path Rel File
configFileName = $(mkRelFile "config.json")

templateDir :: Path Rel Dir
templateDir = $(mkRelDir ".ants-template")

defaultTemplateFileName :: Path Rel File
defaultTemplateFileName = $(mkRelFile "default.md")

hasSubdir :: Path Abs Dir -> Path Rel Dir -> IO Bool
hasSubdir root target = doesDirExist (root </> target)

findDir :: Path Abs Dir -> Path Rel Dir -> IO (Maybe (Path Abs Dir))
findDir dir needle = do
  ifM
    (hasSubdir dir needle)
    (pure (Just dir))
    ( if dir == parent dir
        then pure Nothing
        else findDir (parent dir) needle
    )

findRoot :: IO (Maybe (Path Abs Dir))
findRoot = do
  cwd <- getCurrentDir
  findDir cwd configDir

findTemplate :: Path Abs Dir -> IO (Maybe (Path Abs Dir))
findTemplate dir = findDir dir templateDir

readConfig :: Path Abs Dir -> IO (Maybe Config)
readConfig pathToRoot = do
  let configPath = pathToRoot </> configDir </> configFileName
  conf <- readFile $ toFilePath configPath
  return $ Data.Aeson.decode $ fromString conf
