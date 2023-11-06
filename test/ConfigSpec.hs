{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ConfigSpec
  ( spec,
  )
where

import Config
import Data.Aeson
import Data.ByteString.Lazy.Internal
import Test.Hspec
import Text.RawString.QQ

decodeConfig :: ByteString -> Maybe Config
decodeConfig = decode

spec :: Spec
spec = describe "parse config File" $ do
  it "parses an empty object as a Config with Nothing for template" $ do
    let input = "{}"
    decodeConfig input `shouldBe` Just (Config Nothing)

  it "parses a Config with an empty Template" $ do
    let input = "{\"template\": {}}"
    decodeConfig input
      `shouldBe` Just
        ( Config
            ( Just
                ( Template
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    []
                )
            )
        )

  it "parses a Config with full Template details" $ do
    let input =
          [r|
      {
        "template":{
          "name": "John",
          "email": "john@example.com",
          "dateFormat": "MM-DD-YYYY",
          "timeFormat": "HH:mm:ss",
          "dateTimeFormat": "MM-DD-YYYY HH:mm:ss",
          "variables": {
            "var1": "value1",
            "var2": "value2"
          }
        }
      }
    |]
    decodeConfig input
      `shouldBe` Just
        ( Config
            ( Just
                ( Template
                    (Just "John")
                    (Just "john@example.com")
                    (Just "MM-DD-YYYY")
                    (Just "HH:mm:ss")
                    (Just "MM-DD-YYYY HH:mm:ss")
                    [("var1", "value1"), ("var2", "value2")]
                )
            )
        )

  it "parses a Config with some Template fields missing" $ do
    let input = "{\"template\": {\"name\": \"John\"}}"
    decodeConfig input
      `shouldBe` Just
        ( Config
            ( Just
                ( Template
                    (Just "John")
                    Nothing
                    Nothing
                    Nothing
                    Nothing
                    []
                )
            )
        )

  it "handles incorrect types within the Template object" $ do
    let input = "{\"template\": {\"name\": null, \"email\": 123}}"
    decodeConfig input `shouldBe` Nothing

  it "handles an unexpected structure" $ do
    let input = "{\"unexpected\": \"structure\"}"
    decodeConfig input `shouldBe` Just (Config {template = Nothing})

  it "handles sytanx error" $ do
    let input = "Not a Json file"
    decodeConfig input `shouldBe` Nothing

  it "handles partially incorrect types within a list of variables" $ do
    let input = "{\"template\": {\"variables\": {\"var1\": 123, \"var2\": \"value2\"}}}"
    decodeConfig input
      `shouldBe` Just
        ( Config
            { template =
                Just
                  ( Template
                      { name = Nothing,
                        email = Nothing,
                        dateFormat = Nothing,
                        timeFormat = Nothing,
                        dateTimeFormat = Nothing,
                        variables = [("var2", "value2")]
                      }
                  )
            }
        )
