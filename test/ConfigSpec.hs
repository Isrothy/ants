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
    let input = [r|{}|]
    decodeConfig input `shouldBe` Just (Config def [])

  it "parses a Config with an empty Template" $ do
    let input = [r|{"template": {}}|]
    decodeConfig input
      `shouldBe` Just
        Config
          { template = def,
            extensions = []
          }

  it "parses a Config with full Template details" $ do
    let input =
          [r|{
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
            }|]
    decodeConfig input
      `shouldBe` Just
        ( Config
            { template =
                ( Template
                    (Just "John")
                    (Just "john@example.com")
                    (Just "MM-DD-YYYY")
                    (Just "HH:mm:ss")
                    (Just "MM-DD-YYYY HH:mm:ss")
                    [("var1", "value1"), ("var2", "value2")]
                ),
              extensions = []
            }
        )

  it "parses a Config with some Template fields missing" $ do
    let input = [r|{"template": {"name": "John"}}|]
    decodeConfig input
      `shouldBe` Just
        Config
          { template = (def {name = Just "John"}),
            extensions = []
          }

  it "handles incorrect types within the Template object" $ do
    let input = [r|{"template": {"name": null, "email": 123}}|]
    decodeConfig input
      `shouldBe` Just
        ( Config
            { template = def,
              extensions = []
            }
        )

  it "handles an unexpected structure" $ do
    let input = [r|{"unexpected": "structure"}|]
    decodeConfig input `shouldBe` Just (Config {template = def, extensions = []})

  it "handles syntax error" $ do
    let input = [r|Not a Json file|]
    decodeConfig input `shouldBe` Nothing

  it "handles partially incorrect types within template" $ do
    let input = [r|{"template": { "name" : 1234 }}|]
    decodeConfig input
      `shouldBe` Just
        ( Config
            { template = def,
              extensions = []
            }
        )

  it "handles partially incorrect types within a list of variables" $ do
    let input = [r|{"template": {"variables": {"var1": 123, "var2": "value2"}}}|]
    decodeConfig input
      `shouldBe` Just
        ( Config
            { template = (def {variables = [("var2", "value2")]}),
              extensions = []
            }
        )

  it "parses a Config with extensions as a list of Text" $ do
    let input =
          [r|{
            "template": {"name": "John"},
            "extensions": ["ext1", "ext2"]
          }|]
    decodeConfig input
      `shouldBe` Just
        Config
          { template = (def {name = Just "John"}),
            extensions = ["ext1", "ext2"]
          }

  it "handles empty extensions array" $ do
    let input =
          [r|{
            "template": {"name": "John"},
            "extensions": []
          }|]
    decodeConfig input
      `shouldBe` Just
        Config
          { template = (def {name = Just "John"}),
            extensions = []
          }

  it "handles non-string types in extensions array" $ do
    let input =
          [r|{
              "extensions": ["valid", 123, "also valid"]
          }|]
    decodeConfig input
      `shouldBe` Just
        ( Config
            { template = def,
              extensions = ["valid", "also valid"]
            }
        )
