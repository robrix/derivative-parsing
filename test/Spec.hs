import qualified Derivative.Parser.Spec
import Test.Hspec

main :: IO ()
main = hspec . parallel $ do
  describe "Derivative.Parser" Derivative.Parser.Spec.spec
