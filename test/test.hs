import qualified LispCodeSpec
import qualified Scheme.ParserSpec

import Test.Hspec


specs :: Spec
specs = do
  LispCodeSpec.specs
  Scheme.ParserSpec.specs

main :: IO ()
main = hspec specs
