module Tokens
  ( Tokens,
    parse,
    encode,
    head_,
    tail_,
    empty,
    last_,
    init_,
    cons,
    snoc,
    concat_,
  )
where

import qualified Data.Attoparsec.ByteString
import qualified Data.ByteString
import qualified Token

newtype Tokens
  = Tokens [Token.Token]
  deriving (Show)

concat_ :: Tokens -> Tokens -> Tokens
concat_ (Tokens a) (Tokens b) =
  Tokens (a ++ b)

empty :: Tokens
empty =
  Tokens []

snoc :: Tokens -> Token.Token -> Tokens
snoc (Tokens tokens) token =
  Tokens (tokens ++ [token])

last_ :: Tokens -> Maybe Token.Token
last_ (Tokens tokens) =
  case tokens of
    [] ->
      Nothing
    oneOrMore ->
      Just $ last oneOrMore

cons :: Token.Token -> Tokens -> Tokens
cons token (Tokens tokens) =
  Tokens (token : tokens)

init_ :: Tokens -> Maybe Tokens
init_ (Tokens tokens) =
  case tokens of
    [] ->
      Nothing
    oneOrMore ->
      Just $ Tokens $ init oneOrMore

parse :: Data.Attoparsec.ByteString.Parser Tokens
parse =
  do
    tokens <- Data.Attoparsec.ByteString.many' Token.parse
    _ <- Data.Attoparsec.ByteString.endOfInput
    return $ Tokens (Token.Start : tokens ++ [Token.End])

encode :: Tokens -> Data.ByteString.ByteString
encode (Tokens tokens) =
  mconcat $ map Token.encode tokens

tail_ :: Tokens -> Maybe Tokens
tail_ (Tokens tokens) =
  case tokens of
    [] ->
      Nothing
    _ : remainder ->
      Just (Tokens remainder)

head_ :: Tokens -> Maybe Token.Token
head_ (Tokens tokens) =
  case tokens of
    [] ->
      Nothing
    top : _ ->
      Just top
