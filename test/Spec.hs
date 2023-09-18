import qualified ParserSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  ParserSpec.parserSpec
