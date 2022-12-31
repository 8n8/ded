module Verbatim (Verbatim, parse, encode) where

import qualified Data.Attoparsec.ByteString
import qualified Data.ByteString
import qualified Data.Word

newtype Verbatim
  = Verbatim Data.ByteString.ByteString
  deriving (Show)

encode :: Verbatim -> Data.ByteString.ByteString
encode (Verbatim v) =
  v

parse :: Data.Attoparsec.ByteString.Parser Verbatim
parse =
  fmap Verbatim $
    Data.Attoparsec.ByteString.takeWhile1 isVerbatimChar

isVerbatimChar :: Data.Word.Word8 -> Bool
isVerbatimChar ch =
  ch /= asciiNewline && ch /= asciiSpace

asciiNewline :: Data.Word.Word8
asciiNewline =
  10

asciiSpace :: Data.Word.Word8
asciiSpace =
  32
