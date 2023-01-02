module Main (main) where

import Buf (Buf, append, get, new)
import Data.ByteString (pack, unpack)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Word (Word8)
import qualified Ded
import qualified Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))
import Prelude (Either (Left, Right), IO, Int, Maybe (Just, Nothing), String, map, return, reverse, show, ($), (+))

main :: IO ()
main =
  Test.Tasty.defaultMain tests

tests :: Test.Tasty.TestTree
tests =
  Test.Tasty.testGroup "Unit tests" $
    map oneTest cases

oneTest :: (String, Text, Text) -> Test.Tasty.TestTree
oneTest (name, input, expected) =
  testCase name $
    do
      buf <- Buf.new
      ( do
          result <- bufFromString buf input
          result @?= Right ()
        )
      ( do
          result <- Ded.format buf
          result @?= Right ()
        )
      eitherActual <- bufToString buf
      eitherActual @?= Right expected

bufFromString :: Buf -> Text -> IO (Either String ())
bufFromString buf str =
  bufFromStringHelp buf (unpack $ encodeUtf8 str)

bufFromStringHelp :: Buf -> [Word8] -> IO (Either String ())
bufFromStringHelp buf words =
  case words of
    [] ->
      return $ Right ()
    top : remainder ->
      do
        result <- append buf top
        case result of
          Nothing ->
            return $ Left "buffer too small"
          Just () ->
            bufFromStringHelp buf remainder

bufToString :: Buf -> IO (Either String Text)
bufToString buf =
  toStringHelp buf [] 0

toStringHelp :: Buf -> [Word8] -> Int -> IO (Either String Text)
toStringHelp buf words index =
  do
    maybeByte <- get buf index
    case maybeByte of
      Nothing ->
        case decodeUtf8' $ pack $ reverse words of
          Left err ->
            return $ Left $ show err
          Right text ->
            return $ Right text
      Just byte ->
        toStringHelp buf (byte : words) (index + 1)

helloWorldFormatted :: Text
helloWorldFormatted =
  "module X exposing (x)\n\
  \\n\
  \\n\
  \x =\n\
  \    0\n\
  \"

helloWorldFormattedVariant :: Text
helloWorldFormattedVariant =
  "module Y exposing (y)\n\
  \\n\
  \\n\
  \y =\n\
  \    0\n\
  \"

cases :: [(String, Text, Text)]
cases =
  [ ( "Hello world formatted, so don't change",
      helloWorldFormatted,
      helloWorldFormatted
    ),
    ( "Hello world formatted variant, so don't change",
      helloWorldFormattedVariant,
      helloWorldFormattedVariant
    ),
    ( "Single trailing whitespace",
      "module X exposing (x) \n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      helloWorldFormatted
    )
  ]
