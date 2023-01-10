module Main (main) where

import qualified Data.ByteString
import qualified Ded
import qualified Test.Tasty
import qualified Test.Tasty.HUnit
import qualified Ast
import qualified Buf

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
    do
    ast <- Ast.new
    buf <- Buf.new
    Buf.fill (bufFiller input) buf
    result <- Ded.format ast buf
    result Test.Tasty.HUnit.@?= Right ()
    got <- Buf.toString
    got Test.Tasty.HUnit.@?= expected

helloWorldFormatted :: Data.ByteString.ByteString
helloWorldFormatted =
  "module X exposing (x)\n\
  \\n\
  \\n\
  \x =\n\
  \    0\n\
  \"

cases ::
  [(String, Data.ByteString.ByteString, Data.ByteString.ByteString)]
cases =
  [ ( "Hello world formatted, so don't change",
      helloWorldFormatted,
      helloWorldFormatted
    )
  ]
