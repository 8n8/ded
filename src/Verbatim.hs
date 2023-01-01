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
  Data.Attoparsec.ByteString.choice
    [ parseVerbatimString,
      fmap Verbatim $
        Data.Attoparsec.ByteString.takeWhile1 isVerbatimChar
    ]

parseVerbatimString :: Data.Attoparsec.ByteString.Parser Verbatim
parseVerbatimString =
  do
    _ <- Data.Attoparsec.ByteString.string "\"\"\""
    content <-
      Data.Attoparsec.ByteString.many' $
        Data.Attoparsec.ByteString.choice
          [ Data.Attoparsec.ByteString.takeWhile1
              (\ch -> ch /= asciiDoubleQuote && ch /= asciiBackslash),
            Data.Attoparsec.ByteString.string "\\\"",
            Data.Attoparsec.ByteString.string "\\\\"
          ]
    _ <- Data.Attoparsec.ByteString.string "\"\"\""
    return $ Verbatim ("\"\"\"" <> mconcat content <> "\"\"\"")

asciiBackslash :: Data.Word.Word8
asciiBackslash =
  92

asciiDoubleQuote :: Data.Word.Word8
asciiDoubleQuote =
  34

isVerbatimChar :: Data.Word.Word8 -> Bool
isVerbatimChar ch =
  ch `Data.ByteString.notElem` " \n="

asciiNewline :: Data.Word.Word8
asciiNewline =
  10

asciiSpace :: Data.Word.Word8
asciiSpace =
  32
