{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Project.Link (spec) where

import Path
import Path.IO
import Project.Link
import Test.Hspec

spec :: Spec
spec = describe "followLink" $ sequential $ do
  it "follows an absolute file path correctly" $ do
    withSystemTempDir "test" $ \dir -> do
      let orig = dir </> $(mkRelFile "orig.md")
      let absPath = dir </> $(mkRelFile "test.md")
      followLink orig (toFilePath absPath) `shouldReturn` Just absPath

  it "follows a relative file path correctly" $ do
    withSystemTempDir "test" $ \dir -> do
      _ <- ensureDir (dir </> $(mkRelDir "some/sub/dir"))
      let orig = dir </> $(mkRelFile "some/sub/dir/orig.md")
      followLink orig "../test.md" `shouldReturn` Just (dir </> $(mkRelFile "some/sub/test.md"))

  it "is unchanged if link is empty" $ do
    withSystemTempDir "test" $ \dir -> do
      _ <- ensureDir (dir </> $(mkRelDir "some/sub/dir"))
      let orig = dir </> $(mkRelFile "some/sub/dir/orig.md")
      followLink orig "" `shouldReturn` Just orig
