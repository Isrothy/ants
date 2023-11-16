import qualified ConfigSpec
import qualified MetadataSpec
import qualified Parser.MarkdownParserSpec
import qualified Parser.MetadataSpec
import qualified Parser.PlaceholderParserSpec
import qualified Parser.YamlMarkd
import qualified TemplateGenSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  Parser.MarkdownParserSpec.spec
  Parser.PlaceholderParserSpec.spec
  Parser.MetadataSpec.spec
  Parser.YamlMarkd.spec
  TemplateGenSpec.spec
  ConfigSpec.spec
  MetadataSpec.spec
  return ()
