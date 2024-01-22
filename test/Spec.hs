import qualified Cli.NewNoteGenSpec
import qualified Model.ConfigSpec
import qualified Model.DocFilterSpec
import qualified Model.MetadataSpec
import qualified Parser.MarkdownSpec
import qualified Parser.MetadataSpec
import qualified Parser.PlaceholderSpec
import qualified Parser.YamlMarkdSpec
import qualified Parser.SearchLangSpec
import qualified Util.FuzzySpec
import Test.Hspec

main :: IO ()
main = hspec $ parallel $ do
  Cli.NewNoteGenSpec.spec
  Model.ConfigSpec.spec
  Model.MetadataSpec.spec
  Model.DocFilterSpec.spec
  Parser.MarkdownSpec.spec
  Parser.MetadataSpec.spec
  Parser.PlaceholderSpec.spec
  Parser.SearchLangSpec.spec
  Parser.YamlMarkdSpec.spec
  Util.FuzzySpec.spec
  return ()
