import qualified MarkdownParserSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  MarkdownParserSpec.markdownAstSpec
  return ()
