import qualified Cli.NewNoteGenSpec
import qualified Model.ConfigSpec
import qualified Model.MetadataSpec
import qualified Parser.MarkdownSpec
import qualified Parser.MetadataSpec
import qualified Parser.PlaceholderSpec
import qualified Parser.YamlMarkdSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  Cli.NewNoteGenSpec.spec
  Model.ConfigSpec.spec
  Model.MetadataSpec.spec
  Parser.MarkdownSpec.spec
  Parser.MetadataSpec.spec
  Parser.PlaceholderSpec.spec
  Parser.YamlMarkdSpec.spec
  return ()
