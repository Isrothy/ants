module TemplateGenSpec
  ( spec,
  )
where

import qualified Data.Text as T
import TemplateGen
import Test.Hspec

splitLinesSpec :: Spec
splitLinesSpec = describe "splitLines" $ do
  it "split lines correctly" $ do
    splitLines (T.pack "Hello, world!") `shouldBe` [T.pack "Hello, world!"]
    splitLines (T.pack "Hello, world!\nHow are you today?\nI'm fine, thank you!") `shouldBe` [T.pack "Hello, world!\n", T.pack "How are you today?\n", T.pack "I'm fine, thank you!"]
    splitLines (T.pack "Hello, world!\r\nHow are you today?\rI'm fine, thank you!") `shouldBe` [T.pack "Hello, world!\r\n", T.pack "How are you today?\r", T.pack "I'm fine, thank you!"]

findPositionSpec :: Spec
findPositionSpec = describe "find position" $ do
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

templateGenSpec :: Spec
templateGenSpec = describe "tempalteGen" $ do
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

  it "handles placeholders within strong text" $ do
    let input = T.pack "This is **strong {placeholder} text**.\n"
    let lookupTable = [(T.pack "placeholder", T.pack "and bold")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` T.pack "This is **strong and bold text**.\n"

  it "handles placeholders within emphasized text" $ do
    let input = T.pack "This is *emphasized {placeholder} text*.\n"
    let lookupTable = [(T.pack "placeholder", T.pack "and italic")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` T.pack "This is *emphasized and italic text*.\n"

  it "handles placeholders within headers" $ do
    let input = T.pack "# Heading with {placeholder}\n"
    let lookupTable = [(T.pack "placeholder", T.pack "Placeholder")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` T.pack "# Heading with Placeholder\n"

  it "handles placeholders within links" $ do
    let input = T.pack "[This is a link with a {placeholder}](http://example.com)\n"
    let lookupTable = [(T.pack "placeholder", T.pack "placeholder")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` T.pack "[This is a link with a placeholder](http://example.com)\n"

  it "handles placeholders within images" $ do
    let input = T.pack "![This is an image with a {placeholder}](http://example.com/image.png)\n"
    let lookupTable = [(T.pack "placeholder", T.pack "placeholder")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` T.pack "![This is an image with a placeholder](http://example.com/image.png)\n"

  it "handles placeholders within lists" $ do
    let input = T.pack "- List item with {placeholder}\n- Another list item\n"
    let lookupTable = [(T.pack "placeholder", T.pack "a placeholder")]
    let output = replacePlaceholders "filename" input lookupTable
    output `shouldBe` T.pack "- List item with a placeholder\n- Another list item\n"

spec :: Spec
spec = do
  splitLinesSpec
  findPositionSpec
  templateGenSpec
