{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Project.ProjectRoot
  ( spec,
  )
where

import Control.Exception (bracket_)
import Path
import Path.Bridge
import Project.ProjectRoot
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

configDir :: Path Rel Dir
configDir = $(mkRelDir ".ants")

spec :: Spec
spec = do
  it "finds the root directory when .ants is in the current directory" $
    withSystemTempDirectory "projectRoot" $ \dir -> do
      currentDir <- parseAbsDir dir
      let setup = do
            createDirectory (currentDir </> configDir)
            setCurrentDirectory currentDir
          cleanup = do
            (dirs, files) <- listDirectory currentDir
            mapM_ removeDirectoryRecursive dirs
            mapM_ removeFile files
      bracket_ setup cleanup $ do
        foundRoot <- findRoot
        foundRoot `shouldBe` Just currentDir

  it "returns Nothing when there's no .ants directory up to the root" $
    withSystemTempDirectory "noProjectRoot" $ \dir -> do
      foundRoot <- findRoot
      foundRoot `shouldBe` Nothing

  it "finds the root directory when .ants is several levels up" $
    withSystemTempDirectory "deepProjectRoot" $ \dir -> do
      rootDir <- parseAbsDir dir
      let setup = do
            createDirectoryIfMissing True (rootDir </> configDir)
            createDirectoryIfMissing True (rootDir </> $(mkRelDir "a/b/c"))
            setCurrentDirectory (rootDir </> $(mkRelDir "a/b/c"))
          cleanup = do
            setCurrentDirectory rootDir
            (dirs, files) <- listDirectory rootDir
            mapM_ removeDirectoryRecursive dirs
            mapM_ removeFile files
      bracket_ setup cleanup $ do
        foundRoot <- findRoot
        foundRoot `shouldBe` Just rootDir
