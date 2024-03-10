{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson qualified
import Data.ByteString.Lazy qualified as BL
import Model.Config
import Path
import Path.IO
import Util.IO

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
readConfig pathToRoot = runMaybeT $ do
  let configPath = pathToRoot </> configDir </> configFileName
  text <- MaybeT $ readFileSafe $ toFilePath configPath
  MaybeT $ return $ Data.Aeson.decode text
