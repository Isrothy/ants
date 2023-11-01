import qualified MarkdownParserSpec
import qualified PlaceholderParserSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  MarkdownParserSpec.markdownAstSpec
  PlaceholderParserSpec.placeholderParserSpec
  return ()
