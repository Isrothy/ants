import qualified Spec.Cli.NewNoteGen
import qualified Spec.Model.Config
import qualified Spec.Model.DocFilter
import qualified Spec.Model.Metadata
import qualified Spec.Parser.Markdown
import qualified Spec.Parser.Metadata
import qualified Spec.Parser.Placeholder
import qualified Spec.Parser.SearchLang
import qualified Spec.Parser.YamlMarkd
import qualified Spec.Util.Fuzzy
import Test.Hspec

main :: IO ()
main = hspec $ parallel $ do
  Spec.Cli.NewNoteGen.spec
  Spec.Model.Config.spec
  Spec.Model.Metadata.spec
  Spec.Model.DocFilter.spec
  Spec.Parser.Markdown.spec
  Spec.Parser.Metadata.spec
  Spec.Parser.Placeholder.spec
  Spec.Parser.SearchLang.spec
  Spec.Parser.YamlMarkd.spec
  Spec.Util.Fuzzy.spec
  return ()
