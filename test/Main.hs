import qualified Spec.Cli.NewNoteGen
import qualified Spec.Data.Text.Encoding.Extra
import qualified Spec.Data.Text.LineBreaker
import qualified Spec.Lsp.Server
import qualified Spec.Model.Config
import qualified Spec.Model.DocQuery.Query
import qualified Spec.Model.DocQuery.Term
import qualified Spec.Model.MarkdownAst
import qualified Spec.Model.Metadata
import qualified Spec.Parser.DocQuery
import qualified Spec.Parser.Frontmatter
import qualified Spec.Parser.Markdown
import qualified Spec.Parser.MarkdownWithFrontmatter
import qualified Spec.Parser.Placeholder
import qualified Spec.Project.DocLoader
import qualified Spec.Project.Link
import qualified Spec.Project.ProjectRoot
import qualified Spec.Util.Fuzzy
import Test.Hspec

main :: IO ()
main = hspec $ parallel $ do
  Spec.Cli.NewNoteGen.spec
  Spec.Data.Text.LineBreaker.spec
  Spec.Data.Text.Encoding.Extra.spec
  Spec.Model.Config.spec
  Spec.Model.Metadata.spec
  Spec.Model.MarkdownAst.spec
  Spec.Model.DocQuery.Term.spec
  Spec.Model.DocQuery.Query.spec
  Spec.Parser.Markdown.spec
  Spec.Parser.Frontmatter.spec
  Spec.Parser.Placeholder.spec
  Spec.Parser.DocQuery.spec
  Spec.Parser.MarkdownWithFrontmatter.spec
  Spec.Util.Fuzzy.spec
  Spec.Project.ProjectRoot.spec
  Spec.Project.DocLoader.spec
  Spec.Project.Link.spec
  Spec.Lsp.Server.spec
  return ()
