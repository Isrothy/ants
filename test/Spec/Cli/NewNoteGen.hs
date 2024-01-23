{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.Cli.NewNoteGen
  ( spec,
  )
where

import Cli.NewNoteGen
import qualified Model.Config as Config
import Test.Hspec

splitLinesSpec :: Spec
splitLinesSpec = describe "splitLines" $ do
  it "split lines correctly" $ do
    splitLines "Hello, world!" `shouldBe` ["Hello, world!"]
    splitLines "Hello, world!\nHow are you today?\nI'm fine, thank you!" `shouldBe` ["Hello, world!\n", "How are you today?\n", "I'm fine, thank you!"]
    splitLines "Hello, world!\r\nHow are you today?\rI'm fine, thank you!" `shouldBe` ["Hello, world!\r\n", "How are you today?\r", "I'm fine, thank you!"]

findPositionSpec :: Spec
findPositionSpec = describe "find position" $ parallel $ do 
  it "finds position correctly in single-line text" $ do
    findPosition 1 1 "Hello, world!" `shouldBe` Just 0
    findPosition 1 2 "Hello, world!" `shouldBe` Just 1
    findPosition 1 13 "Hello, world!" `shouldBe` Just 12
    findPosition 1 14 "Hello, world!" `shouldBe` Nothing
    findPosition 2 1 "Hello, world!" `shouldBe` Nothing

  it "handles empty text correctly" $ do
    findPosition 1 1 "" `shouldBe` Nothing
    findPosition 1 2 "" `shouldBe` Nothing
    findPosition 2 1 "" `shouldBe` Nothing

  it "handles multi-line text correctly" $ do
    let text = "Hello, world!\nHow are you today?\nI'm fine, thank you!"
    findPosition 1 1 text `shouldBe` Just 0
    findPosition 1 14 text `shouldBe` Just 13
    findPosition 2 1 text `shouldBe` Just 14
    findPosition 2 5 text `shouldBe` Just 18
    findPosition 3 18 text `shouldBe` Just 50
    findPosition 4 1 text `shouldBe` Nothing

  it "handles special characters correctly" $ do
    let text = "Hëllo, wørld!\nHow are you today? 你好吗？\nI'm fine, thank you! 😊"
    findPosition 1 6 text `shouldBe` Just 5
    findPosition 2 23 text `shouldBe` Just 36
    findPosition 3 22 text `shouldBe` Just 59
    findPosition 3 23 text `shouldBe` Nothing

  it "handles DOS and Mac line endings correctly" $ do
    let dosText = "Hello, world!\r\nHow are you today?\r\nI'm fine, thank you!"
    let macText = "Hello, world!\rHow are you today?\rI'm fine, thank you!"
    findPosition 2 1 dosText `shouldBe` Just 15
    findPosition 2 1 macText `shouldBe` Just 14

templateGenSpec :: Spec
templateGenSpec = describe "tempalteGen" $ parallel $ do
  it "replace single placeholders correctly" $ do
    let input = "Hello, world! This is a {placeholder}.\n"
    let lookupTable = [("placeholder", "replacement")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` "Hello, world! This is a replacement.\n"

  it "replace multiple placeholders correctly" $ do
    let input = "Hello, {user}!\nWelcome to {platform}.\nEnjoy your stay!\n"
    let lookupTable = [("user", "John Doe"), ("platform", "StackOverflow")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` "Hello, John Doe!\nWelcome to StackOverflow.\nEnjoy your stay!\n"

  it "handles placeholders at the start and end correctly" $ do
    let input = "{greeting} world! This is a test {placeholder}.\n"
    let lookupTable = [("greeting", "Hello,"), ("placeholder", "example")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` "Hello, world! This is a test example.\n"

  it "does not replace non-existent placeholders" $ do
    let input = "This text has a {nonexistent} placeholder.\n"
    let lookupTable = [("placeholder", "replacement")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` input

  it "handles consecutive placeholders" $ do
    let input = "Consecutive placeholders: {one}{two}{three}\n"
    let lookupTable = [("one", "1"), ("two", "2"), ("three", "3")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` "Consecutive placeholders: 123\n"

  it "ignores escaped placeholders" $ do
    let input = "This is an escaped \\{placeholder}.\n"
    let lookupTable = [("placeholder", "should not be replaced")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` input

  it "handles placeholders with special characters" $ do
    let input = "Special characters: {placeholder_123}!\n"
    let lookupTable = [("placeholder_123", "replaced")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` "Special characters: replaced!\n"

  it "handles line breaks correctly" $ do
    let input = "This is a test with a\n{placeholder}\nacross lines.\n"
    let lookupTable = [("placeholder", "replacement")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` "This is a test with a\nreplacement\nacross lines.\n"

  it "handles placeholders within strong text" $ do
    let input = "This is **strong {placeholder} text**.\n"
    let lookupTable = [("placeholder", "and bold")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` "This is **strong and bold text**.\n"

  it "handles placeholders within emphasized text" $ do
    let input = "This is *emphasized {placeholder} text*.\n"
    let lookupTable = [("placeholder", "and italic")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` "This is *emphasized and italic text*.\n"

  it "handles placeholders within headers" $ do
    let input = "# Heading with {placeholder}\n"
    let lookupTable = [("placeholder", "Placeholder")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` "# Heading with Placeholder\n"

  it "handles placeholders within links" $ do
    let input = "[This is a link with a {placeholder}](http://example.com)\n"
    let lookupTable = [("placeholder", "placeholder")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` "[This is a link with a placeholder](http://example.com)\n"

  it "handles placeholders within images" $ do
    let input = "![This is an image with a {placeholder}](http://example.com/image.png)\n"
    let lookupTable = [("placeholder", "placeholder")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` "![This is an image with a placeholder](http://example.com/image.png)\n"

  it "handles placeholders within lists" $ do
    let input = "- List item with {placeholder}\n- Another list item\n"
    let lookupTable = [("placeholder", "a placeholder")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` "- List item with a placeholder\n- Another list item\n"

  it "handles case insensitive placeholders" $ do
    let input = "This is a {PlAcEhOlDeR} with mixed case letters.\n"
    let lookupTable = [("placeholder", "replacement")]
    let output = replacePlaceholders mempty "filename" input lookupTable
    output `shouldBe` "This is a replacement with mixed case letters.\n"

configToLookupTableSpec :: Spec
configToLookupTableSpec = describe "fromConfig" $ parallel $ do
  it "uses default formats when specific formats are not provided" $ do
    let template =
          Config.Template
            { Config.name = Just "John",
              Config.email = Just "john@example.com",
              Config.dateFormat = Nothing,
              Config.timeFormat = Nothing,
              Config.dateTimeFormat = Nothing,
              Config.variables = []
            }
    let config = Config.Config {Config.template = template, Config.extensions = []}
    fromConfig config
      `shouldBe` [ ("name", "John"),
                   ("email", "john@example.com"),
                   ("date", "%Y-%m-%d"),
                   ("time", "%H:%M:%S"),
                   ("dateTime", "%Y-%m-%dT%H:%M:%S")
                 ]

  it "uses provided formats" $ do
    let template =
          Config.Template
            { Config.name = Just "Joshua",
              Config.email = Just "Joshua@example.com",
              Config.dateFormat = Just "MM-DD-YYYY",
              Config.timeFormat = Just "HH:mm:ss",
              Config.dateTimeFormat = Just "MM-DD-YYYY HH:mm:ss",
              Config.variables = [("var1", "value1"), ("var2", "value2")]
            }
    let config = Config.Config {Config.template = template, Config.extensions = []}
    fromConfig config
      `shouldBe` [ ("name", "Joshua"),
                   ("email", "Joshua@example.com"),
                   ("date", "MM-DD-YYYY"),
                   ("time", "HH:mm:ss"),
                   ("dateTime", "MM-DD-YYYY HH:mm:ss"),
                   ("var1", "value1"),
                   ("var2", "value2")
                 ]

  it "handles missing fields correctly" $ do
    let template =
          Config.Template
            { Config.name = Nothing,
              Config.email = Nothing,
              Config.dateFormat = Nothing,
              Config.timeFormat = Nothing,
              Config.dateTimeFormat = Nothing,
              Config.variables = []
            }
    let config =
          Config.Config
            { Config.template = template,
              Config.extensions = []
            }
    fromConfig config
      `shouldBe` [ ("date", "%Y-%m-%d"),
                   ("time", "%H:%M:%S"),
                   ("dateTime", "%Y-%m-%dT%H:%M:%S")
                 ]

spec :: Spec
spec = parallel $ do
  splitLinesSpec
  findPositionSpec
  templateGenSpec
  configToLookupTableSpec
