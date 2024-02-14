{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Project.ProjectRoot
  ( spec,
  )
where

import Control.Exception (bracket_)
import Path
import Path.IO
import Project.ProjectRoot
import Test.Hspec

spec :: Spec
spec = describe "project root" $ sequential $ do
  it "finds the root directory when .ants is in the current directory" $
    withSystemTempDir "projectRoot" $ \tmp -> do
      cur <- getCurrentDir
      let setup = do
            ensureDir (tmp </> configDir)
            setCurrentDir tmp
          cleanup = do
            setCurrentDir cur
      bracket_ setup cleanup $ do
        foundRoot <- findRoot
        foundRoot `shouldBe` Just tmp

  it "returns Nothing when there's no .ants directory up to the root" $
    withSystemTempDir "noProjectRoot" $ \dir -> do
      foundRoot <- findRoot
      foundRoot `shouldBe` Nothing

  it "finds the root directory when .ants is several levels up" $
    withSystemTempDir "deepProjectRoot" $ \tmp -> do
      cur <- getCurrentDir
      let setup = do
            ensureDir (tmp </> configDir)
            ensureDir (tmp </> $(mkRelDir "a/b/c"))
            setCurrentDir (tmp </> $(mkRelDir "a/b/c"))
          cleanup = do
            setCurrentDir cur
      bracket_ setup cleanup $ do
        foundRoot <- findRoot
        foundRoot `shouldBe` Just tmp
