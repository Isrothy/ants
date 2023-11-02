import qualified MarkdownParserSpec
import qualified PlaceholderParserSpec
import qualified TemplateGenSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  MarkdownParserSpec.markdownAstSpec
  PlaceholderParserSpec.placeholderParserSpec
  TemplateGenSpec.templateGenSpec
  return ()
