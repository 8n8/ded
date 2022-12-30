import qualified Test.Tasty
import qualified Test.Tasty.SmallCheck as SC
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit
import qualified Data.Text
import qualified Ded

main = Test.Tasty.defaultMain tests

tests :: Test.Tasty.TestTree
tests = Test.Tasty.testGroup "Unit tests"
  [ Test.Tasty.HUnit.testCase "Hello world formatted" $
      (Ded.format helloWorldFormatted)
        Test.Tasty.HUnit.@?= helloWorldFormatted
  ]


helloWorldFormatted :: Data.Text.Text
helloWorldFormatted =
  "module X exposing (x)\n\
  \\n\
  \\n\
  \x =\n\
  \    0\n\
  \"


