module Token (Token (..), parse, encode) where

import qualified Data.Attoparsec.ByteString
import qualified Data.ByteString
import qualified Data.Word
import qualified Verbatim

data Token
  = Newline
  | Space
  | Equals
  | Verbatim Verbatim.Verbatim
  | Start
  | End
  deriving (Show)

asciiNewline :: Data.Word.Word8
asciiNewline =
  10

asciiSpace :: Data.Word.Word8
asciiSpace =
  32

parse :: Data.Attoparsec.ByteString.Parser Token
parse =
  Data.Attoparsec.ByteString.choice
    [ parseNewline,
      parseSpace,
      parseEquals,
      fmap Verbatim Verbatim.parse
    ]

parseNewline :: Data.Attoparsec.ByteString.Parser Token
parseNewline =
  do
    _ <- Data.Attoparsec.ByteString.word8 asciiNewline
    return Newline

parseEquals :: Data.Attoparsec.ByteString.Parser Token
parseEquals =
  do
    _ <- Data.Attoparsec.ByteString.word8 asciiEquals
    return Equals

asciiEquals :: Data.Word.Word8
asciiEquals =
  61

parseSpace :: Data.Attoparsec.ByteString.Parser Token
parseSpace =
  do
    _ <- Data.Attoparsec.ByteString.word8 asciiSpace
    return Space

encode :: Token -> Data.ByteString.ByteString
encode token =
  case token of
    Newline ->
      "\n"
    Space ->
      " "
    Verbatim verbatim ->
      Verbatim.encode verbatim
    Start ->
      ""
    End ->
      ""
    Equals ->
      "="
