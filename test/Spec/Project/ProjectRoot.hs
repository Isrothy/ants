{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Project.ProjectRoot
  ( spec,
  )
where

import Control.Exception (bracket_)
import Path
import Project.ProjectRoot
import System.Directory
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
            createDirectory (toFilePath (currentDir </> configDir))
            setCurrentDirectory dir
          cleanup = do
            sub <- listDirectory dir
            mapM_ removeDirectoryRecursive sub
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
            createDirectoryIfMissing True (toFilePath (rootDir </> configDir))
            createDirectoryIfMissing True (toFilePath (rootDir </> $(mkRelDir "a/b/c")))
            setCurrentDirectory $ toFilePath (rootDir </> $(mkRelDir "a/b/c"))
          cleanup = do
            setCurrentDirectory dir
            sub <- listDirectory dir
            mapM_ removeDirectoryRecursive sub
      bracket_ setup cleanup $ do
        foundRoot <- findRoot
        foundRoot `shouldBe` Just rootDir
