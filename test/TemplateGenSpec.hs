module TemplateGenSpec
  ( templateGenSpec,
  )
where

import qualified Data.Text as T
import TemplateGen
import Test.Hspec

templateGenSpec :: Spec
templateGenSpec = describe "tempalteGen" $ do
  it "split lines correctly" $ do
    splitLines (T.pack "Hello, world!") `shouldBe` [T.pack "Hello, world!"]
    splitLines (T.pack "Hello, world!\nHow are you today?\nI'm fine, thank you!") `shouldBe` [T.pack "Hello, world!\n", T.pack "How are you today?\n", T.pack "I'm fine, thank you!"]
    splitLines (T.pack "Hello, world!\r\nHow are you today?\rI'm fine, thank you!") `shouldBe` [T.pack "Hello, world!\r\n", T.pack "How are you today?\r", T.pack "I'm fine, thank you!"]

  it "finds position correctly in single-line text" $ do
    findPosition 1 1 (T.pack "Hello, world!") `shouldBe` Just 0
    findPosition 1 2 (T.pack "Hello, world!") `shouldBe` Just 1
    findPosition 1 13 (T.pack "Hello, world!") `shouldBe` Just 12
    findPosition 1 14 (T.pack "Hello, world!") `shouldBe` Nothing
    findPosition 2 1 (T.pack "Hello, world!") `shouldBe` Nothing

  it "handles empty text correctly" $ do
    findPosition 1 1 (T.pack "") `shouldBe` Nothing
    findPosition 1 2 (T.pack "") `shouldBe` Nothing
    findPosition 2 1 (T.pack "") `shouldBe` Nothing

  it "handles multi-line text correctly" $ do
    let text = T.pack "Hello, world!\nHow are you today?\nI'm fine, thank you!"
    findPosition 1 1 text `shouldBe` Just 0
    findPosition 1 14 text `shouldBe` Just 13
    findPosition 2 1 text `shouldBe` Just 14
    findPosition 2 5 text `shouldBe` Just 18
    findPosition 3 18 text `shouldBe` Just 50
    findPosition 4 1 text `shouldBe` Nothing

  it "handles special characters correctly" $ do
    let text = T.pack "Hëllo, wørld!\nHow are you today? 你好吗？\nI'm fine, thank you! 😊"
    findPosition 1 6 text `shouldBe` Just 5
    findPosition 2 23 text `shouldBe` Just 36
    findPosition 3 22 text `shouldBe` Just 59
    findPosition 3 23 text `shouldBe` Nothing

  it "handles DOS and Mac line endings correctly" $ do
    let dosText = T.pack "Hello, world!\r\nHow are you today?\r\nI'm fine, thank you!"
    let macText = T.pack "Hello, world!\rHow are you today?\rI'm fine, thank you!"
    findPosition 2 1 dosText `shouldBe` Just 15
    findPosition 2 1 macText `shouldBe` Just 14

  it "replace single placeholders correctly" $ do
    let input = T.pack "Hello, world! This is a {placeholder}.\n"
    let lookupTable = [(T.pack "placeholder", T.pack "replacement")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` T.pack "Hello, world! This is a replacement.\n"

  it "replace multiple placeholders correctly" $ do
    let input = T.pack "Hello, {user}! Welcome to {platform}. Enjoy your stay!\n"
    let lookupTable = [(T.pack "user", T.pack "John Doe"), (T.pack "platform", T.pack "StackOverflow")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` T.pack "Hello, John Doe! Welcome to StackOverflow. Enjoy your stay!\n"

  it "handles placeholders at the start and end correctly" $ do
    let input = T.pack "{greeting} world! This is a test {placeholder}.\n"
    let lookupTable = [(T.pack "greeting", T.pack "Hello,"), (T.pack "placeholder", T.pack "example")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` T.pack "Hello, world! This is a test example.\n"

  it "does not replace non-existent placeholders" $ do
    let input = T.pack "This text has a {nonexistent} placeholder.\n"
    let lookupTable = [(T.pack "placeholder", T.pack "replacement")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` input

  it "handles consecutive placeholders" $ do
    let input = T.pack "Consecutive placeholders: {one}{two}{three}\n"
    let lookupTable = [(T.pack "one", T.pack "1"), (T.pack "two", T.pack "2"), (T.pack "three", T.pack "3")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` T.pack "Consecutive placeholders: 123\n"

  it "ignores escaped placeholders" $ do
    let input = T.pack "This is an escaped \\{placeholder}.\n"
    let lookupTable = [(T.pack "placeholder", T.pack "should not be replaced")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` input

  it "handles placeholders with special characters" $ do
    let input = T.pack "Special characters: {placeholder_123}!\n"
    let lookupTable = [(T.pack "placeholder_123", T.pack "replaced")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` T.pack "Special characters: replaced!\n"

  it "handles line breaks correctly" $ do
    let input = T.pack "This is a test with a\n{placeholder}\nacross lines.\n"
    let lookupTable = [(T.pack "placeholder", T.pack "replacement")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` T.pack "This is a test with a\nreplacement\nacross lines.\n"
