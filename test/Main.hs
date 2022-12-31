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
    , Test.Tasty.HUnit.testCase "Remove single trailing whitespace" $
        (Ded.format helloWorldTrailingWhitespace)
          Test.Tasty.HUnit.@?= (Right helloWorldFormatted)
    , Test.Tasty.HUnit.testCase "Remove double trailing whitespace" $
        (Ded.format helloWorldTwoTrailingWhitespace)
          Test.Tasty.HUnit.@?= (Right helloWorldFormatted)
    ]

helloWorldTrailingWhitespace :: Data.ByteString.ByteString
helloWorldTrailingWhitespace =
  "module X exposing (x) \n\
  \\n\
  \\n\
  \x =\n\
  \    0\n\
  \"

helloWorldTwoTrailingWhitespace :: Data.ByteString.ByteString
helloWorldTwoTrailingWhitespace =
  "module X exposing (x)  \n\
  \\n\
  \\n\
  \x =\n\
  \    0\n\
  \"

helloWorldFormatted :: Data.ByteString.ByteString
helloWorldFormatted =
  "module X exposing (x)\n\
  \\n\
  \\n\
  \x =\n\
  \    0\n\
  \"
