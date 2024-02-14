{-# LANGUAGE TemplateHaskell #-}

module Project.ProjectRoot (findRoot, findTemplate, configDir, templateDir, configFileName, defaultTemplateFileName) where

import Control.Conditional
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
