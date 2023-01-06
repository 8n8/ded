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
  | OpenBracket
  | CloseBracket
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
      parseOpenBracket,
      parseCloseBracket,
      fmap Verbatim Verbatim.parse
    ]

parseOpenBracket :: Data.Attoparsec.ByteString.Parser Token
parseOpenBracket =
    do
    _ <- Data.Attoparsec.ByteString.word8 asciiOpenBracket
    return OpenBracket

parseCloseBracket :: Data.Attoparsec.ByteString.Parser Token
parseCloseBracket =
    do
    _ <- Data.Attoparsec.ByteString.word8 asciiCloseBracket
    return CloseBracket

asciiOpenBracket :: Data.Word.Word8
asciiOpenBracket =
    91

asciiCloseBracket :: Data.Word.Word8
asciiCloseBracket =
    93

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

    OpenBracket ->
        "["

    CloseBracket ->
        "]"
