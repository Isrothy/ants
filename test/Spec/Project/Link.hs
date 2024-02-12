{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Spec.Project.Link (spec) where

import qualified Data.Text as T
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
      followLink orig (toFilePath absPath) `shouldReturn` absPath

  it "follows a relative file path correctly" $ do
    withSystemTempDir "test" $ \dir -> do
      _ <- ensureDir (dir </> $(mkRelDir "some/sub/dir"))
      let orig = dir </> $(mkRelFile "some/sub/dir/orig.md")
      followLink orig "../test.md" `shouldReturn` (dir </> $(mkRelFile "some/sub/test.md"))

-- Additional tests can include scenarios with permissions issues, malformed paths, etc.
