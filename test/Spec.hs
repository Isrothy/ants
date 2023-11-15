import qualified ConfigSpec
import qualified Parser.MarkdownParserSpec
import qualified Parser.MetadataSpec
import qualified Parser.PlaceholderParserSpec
import qualified TemplateGenSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  Parser.MarkdownParserSpec.spec
  Parser.PlaceholderParserSpec.spec
  Parser.MetadataSpec.spec
  TemplateGenSpec.spec
  ConfigSpec.spec
  return ()
