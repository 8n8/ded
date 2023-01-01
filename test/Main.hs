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
  Test.Tasty.testGroup "Unit tests" $
    map oneTest cases

oneTest :: (String, Data.ByteString.ByteString, Data.ByteString.ByteString) -> Test.Tasty.TestTree
oneTest (name, input, expected) =
  Test.Tasty.HUnit.testCase name $
    (Ded.format input) Test.Tasty.HUnit.@?= (Right expected)

helloWorldFormatted :: Data.ByteString.ByteString
helloWorldFormatted =
  "module X exposing (x)\n\
  \\n\
  \\n\
  \x =\n\
  \    0\n\
  \"

helloWorldVariantFormatted :: Data.ByteString.ByteString
helloWorldVariantFormatted =
  "module Y exposing (y)\n\
  \\n\
  \\n\
  \y =\n\
  \    0\n\
  \"

cases ::
  [(String, Data.ByteString.ByteString, Data.ByteString.ByteString)]
cases =
  [ ( "Hello world formatted, so don't change",
      helloWorldFormatted,
      helloWorldFormatted
    ),
    ( "Hello world variant formatted, so don't change",
      helloWorldVariantFormatted,
      helloWorldVariantFormatted
    )
  ]
