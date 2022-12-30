module Main (main) where

import qualified Data.ByteString
import qualified Ded
import qualified Test.Tasty
import qualified Test.Tasty.HUnit

main :: IO ()
main =
  Test.Tasty.defaultMain tests

tests :: Test.Tasty.TestTree
tests =
  Test.Tasty.testGroup
    "Unit tests"
    [ Test.Tasty.HUnit.testCase "Hello world formatted" $
        (Ded.format helloWorldFormatted)
          Test.Tasty.HUnit.@?= (Right helloWorldFormatted)
    ]

helloWorldFormatted :: Data.ByteString.ByteString
helloWorldFormatted =
  "module X exposing (x)\n\
  \\n\
  \\n\
  \x =\n\
  \    0\n\
  \"
