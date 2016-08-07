import qualified Derivative.Lexer.Spec
import qualified Derivative.Parser.Spec
import Test.Hspec

main :: IO ()
main = hspec . parallel $ do
  describe "Derivative.Lexer" Derivative.Lexer.Spec.spec
  describe "Derivative.Parser" Derivative.Parser.Spec.spec
