module ParserSpec
  ( parserSpec,
  )
where

import Data.Char
import Parser.Main
import Test.Hspec

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

testPositionUpdate :: Spec
testPositionUpdate = describe "Position update" $ do
  it "updates column on char parse" $ do
    let result = runParser (char 'a') (Position 1 1) "abc"
    case result of
      Right (_, newPos, _) -> newPos `shouldBe` Position 1 2
      Left _ -> expectationFailure "Expected successful parse"

  it "updates line on newline parse" $ do
    let result = runParser (char '\n') (Position 1 1) "\nabc"
    case result of
      Right (_, newPos, _) -> newPos `shouldBe` Position 2 1
      Left _ -> expectationFailure "Expected successful parse"

testChar :: Spec
testChar = describe "char parser" $ do
  it "parses matching character" $
    runParser (char 'a') (Position 1 1) "abc" `shouldBe` Right ('a', Position 1 2, "bc")

  it "gives error on non-matching character" $
    runParser (char 'b') (Position 1 1) "adc" `shouldSatisfy` isLeft

  it "gives error on empty input" $
    runParser (char 'a') (Position 1 1) "" `shouldSatisfy` isLeft

testAnyChar :: Spec
testAnyChar = describe "anyChar parser" $ do
  it "parses any character" $
    runParser anyChar (Position 1 1) "abc" `shouldBe` Right ('a', Position 1 2, "bc")

  it "gives error on empty input" $
    runParser anyChar (Position 1 1) "" `shouldSatisfy` isLeft

testString :: Spec
testString = describe "string parser" $ do
  it "parses matching string" $
    runParser (string "abc") (Position 1 1) "abcdef" `shouldBe` Right ("abc", Position 1 4, "def")

  it "gives error on partially matching string" $
    runParser (string "abcd") (Position 1 1) "abc" `shouldSatisfy` isLeft
  it "gives error on non-matching string" $
    runParser (string "abc") (Position 1 1) "abdef" `shouldSatisfy` isLeft

  it "parses empty string with empty input" $
    runParser (string "") (Position 1 1) "" `shouldBe` Right ("", Position 1 1, "")

  it "gives error on empty input" $
    runParser (string "abc") (Position 1 1) "" `shouldSatisfy` isLeft

testOneOf :: Spec
testOneOf = describe "oneOf parser" $ do
  it "parses one of the characters" $
    runParser (oneOf "abc") (Position 1 1) "bcd" `shouldBe` Right ('b', Position 1 2, "cd")

  it "gives error on non-matching character" $
    runParser (oneOf "abc") (Position 1 1) "def" `shouldSatisfy` isLeft

  it "gives error on empty input" $
    runParser (oneOf "abc") (Position 1 1) "" `shouldSatisfy` isLeft

testManyAndSome :: Spec
testManyAndSome = describe "many and some parsers" $ do
  it "parses many characters" $
    runParser (many (char 'a')) (Position 1 1) "aaa" `shouldBe` Right ("aaa", Position 1 4, "")

  it "parses some characters" $
    runParser (some (char 'a')) (Position 1 1) "aaa" `shouldBe` Right ("aaa", Position 1 4, "")

  it "stops parsing when many doesn't match" $
    runParser (many (char 'a')) (Position 1 1) "aaabbb" `shouldBe` Right ("aaa", Position 1 4, "bbb")

  it "stops parsing when some doesn't match" $
    runParser (some (char 'a')) (Position 1 1) "aaabbb" `shouldBe` Right ("aaa", Position 1 4, "bbb")

  it "gives error on non-matching some" $
    runParser (some (char 'a')) (Position 1 1) "bbb" `shouldSatisfy` isLeft

  it "gives no error on non-matching many" $
    runParser (many (char 'a')) (Position 1 1) "bbb" `shouldBe` Right ("", Position 1 1, "bbb")

testDoNotation :: Spec
testDoNotation = describe "Do notation in Parser monad" $ do
  it "parses two characters in sequence" $ do
    let parser = do
          a <- char 'a'
          b <- char 'b'
          return (a, b)
    runParser parser (Position 1 1) "abc" `shouldBe` Right (('a', 'b'), Position 1 3, "c")

  it "parses a digit followed by an alphabetic character" $ do
    let parser = do
          d <- satisfy isDigit
          a <- satisfy isAlpha
          return (d, a)
    runParser parser (Position 1 1) "1a2" `shouldBe` Right (('1', 'a'), Position 1 3, "2")

  it "fails to parse two characters in sequence if the second doesn't match" $ do
    let parser = do
          a <- char 'a'
          b <- char 'c'
          return (a, b)
    let result = runParser parser (Position 1 1) "abc"
    case result of
      Left (Mismatch expected found _) -> do
        expected `shouldBe` 'c'
        found `shouldBe` 'b'
      _ -> expectationFailure "Expected a mismatch error but received a different result"

testEither :: Spec
testEither = describe "<|> alternative parser" $ do
  it "chooses the first parser when it matches" $
    runParser (char 'a' <|> char 'b') (Position 1 1) "abc" `shouldBe` Right ('a', Position 1 2, "bc")

  it "chooses the second parser if the first fails" $
    runParser (char 'b' <|> char 'a') (Position 1 1) "abc" `shouldBe` Right ('a', Position 1 2, "bc")

  it "fails if both parsers fail" $
    runParser (char 'b' <|> char 'c') (Position 1 1) "abc" `shouldSatisfy` isLeft

testAlwaysFail :: Spec
testAlwaysFail = do
  describe "alwaysFail parser" $ do
    it "always fails" $
      runParser ((alwaysFail :: String -> Parser Int) "error") (Position 1 1) "abc"
        `shouldSatisfy` isLeft

    it "gives the error message" $
      runParser ((alwaysFail :: String -> Parser Int) "error") (Position 1 1) "abc"
        `shouldBe` Left (CustomError "error")

testDigits :: Spec
testDigits = describe "digits parser" $ do
  it "parses digits" $
    runParser digits (Position 1 1) "1223abc" `shouldBe` Right ("1223", Position 1 5, "abc")

  it "gives error on non-matching digits" $
    runParser digits (Position 1 1) "abc" `shouldSatisfy` isLeft

testSpace :: Spec
testSpace = do
  describe "spaces parser" $ do
    it "parses spaces" $
      runParser spaces (Position 1 1) "  \t\n " `shouldBe` Right ("  \t\n ", Position 2 2, "")

    it "gives no error on non-matching spaces" $
      runParser spaces (Position 1 1) "abc" `shouldBe` Right ("", Position 1 1, "abc")

testAlphaNum :: Spec
testAlphaNum = do
  describe "alphaNum parser" $ do
    it "parses alphanumeric character" $
      runParser alphaNum (Position 1 1) "a223" `shouldBe` Right ('a', Position 1 2, "223")

    it "gives error on non-alphanumeric character" $
      runParser alphaNum (Position 1 1) "#223" `shouldSatisfy` isLeft

parserSpec :: Spec
parserSpec = do
  testPositionUpdate
  testChar
  testAnyChar
  testString
  testOneOf
  testManyAndSome
  testDoNotation
  testEither
  testAlwaysFail
  testDigits
  testSpace
  testAlphaNum
