{-# LANGUAGE TemplateHaskell #-}

module Project.ProjectRoot (findRoot) where

import Control.Conditional
import Path
import System.Directory

configDir :: Path Rel Dir
configDir = $(mkRelDir ".ants")

hasSubdir :: Path Abs Dir -> Path Rel Dir -> IO Bool
hasSubdir root target = doesDirectoryExist (toFilePath (root </> target))

findRoot :: IO (Maybe (Path Abs Dir))
findRoot = do
  cwd <- getCurrentDirectory >>= parseAbsDir
  helper cwd
  where
    helper dir = do
      ifM
        (hasSubdir dir configDir)
        (pure (Just dir))
        ( if dir == $(mkAbsDir "/")
            then pure Nothing
            else helper (parent dir)
        )
