{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MetadataSpec
  ( spec,
  )
where

import Common.Default
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Format.ISO8601
import qualified Data.Yaml as Y
import Metadata
import Test.Hspec
import Text.RawString.QQ

decodeMaybeMetadata :: String -> Maybe Metadata
decodeMaybeMetadata str = case Y.decodeEither' (TE.encodeUtf8 $ T.pack str) of
  Left _ -> Nothing
  Right metadata -> Just metadata

spec :: Spec
spec = describe "parse metadata File" $ do
  it "parses metadata with all fields" $ do
    let input =
          [r|
title: Example Title
author: Joshua
dateTime: 2023-03-01T11:22:33Z
tags:
  - Haskell
  - Parsing
description: Description of the document
|]
    let expectedResult = Metadata (Just "Example Title") (Just "Joshua") (iso8601ParseM "2023-03-01T11:22:33Z") ["Haskell", "Parsing"] "Description of the document"
    decodeMaybeMetadata input `shouldBe` Just expectedResult

  it "handles missing optional fields" $ do
    let input =
          [r|
title: Only Title
tags:
  - Single Tag
description: Only description
|]
    let expectedResult = Metadata (Just "Only Title") Nothing Nothing ["Single Tag"] "Only description"
    decodeMaybeMetadata input `shouldBe` Just expectedResult

  it "parses empty metadata" $ do
    let input =
          [r|
description: ""
|]
    let expectedResult = def {description = ""}
    decodeMaybeMetadata input `shouldBe` Just expectedResult
  it "handles incorrect data types for fields" $ do
    let input =
          [r|
title: 12345
author: ["Jane", "Doe"]
dateTime: true
tags:
  - true
  - 123
description: 1000
|]
    decodeMaybeMetadata input `shouldBe` Just def

  it "ignores non-string values in tags array" $ do
    let input =
          [r|
title: Example Title
author: Joshua
dateTime: 2023-03-01T11:22:33Z
tags:
  - Haskell
  - 123
  - Parsing
description: A document
|]
    let expectedResult = Metadata (Just "Example Title") (Just "Joshua") (iso8601ParseM "2023-03-01T11:22:33Z") ["Haskell", "Parsing"] "A document"
    decodeMaybeMetadata input `shouldBe` Just expectedResult
