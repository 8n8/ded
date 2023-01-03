module Gap (Gap, init_, read_, update, Action (..), encode) where

import qualified Data.Attoparsec.ByteString
import qualified Data.ByteString
import qualified Token
import qualified Tokens

data Gap
  = Gap Tokens.Tokens Token.Token Tokens.Tokens
  deriving (Show)

encode :: Gap -> Data.ByteString.ByteString
encode (Gap left token right) =
  Tokens.encode
    (Tokens.concat_ left (Tokens.cons token right))

invalidElmFile :: String
invalidElmFile =
  "invalid Elm file"

data Action
  = MoveRight
  | MoveLeft
  | Delete
  | InsertSpace
  | InsertNewline
  | MoveRightTwice
  | DoNothing
  deriving (Show)

maybeMap2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeMap2 f maybeA maybeB =
  case (maybeA, maybeB) of
    (Nothing, Just _) ->
      Nothing
    (Nothing, Nothing) ->
      Nothing
    (Just _, Nothing) ->
      Nothing
    (Just a, Just b) ->
      Just (f a b)

init_ :: Data.ByteString.ByteString -> Either String Gap
init_ raw =
  case Data.Attoparsec.ByteString.parseOnly Tokens.parse raw of
    Left err ->
      Left err
    Right tokens ->
      case maybeMap2
        (\cursor right -> Gap Tokens.empty cursor right)
        (Tokens.head_ tokens)
        (Tokens.tail_ tokens) of
        Nothing ->
          Left invalidElmFile
        Just gap ->
          Right gap

read_ :: Gap -> Token.Token
read_ (Gap _ cursor _) =
  cursor

update :: Action -> Gap -> Maybe Gap
update action gap =
  case action of
    MoveLeft ->
      moveLeft gap
    MoveRight ->
      moveRight gap
    MoveRightTwice ->
      case moveRight gap of
        Nothing ->
          Nothing
        Just newGap ->
          moveRight newGap
    Delete ->
      delete gap
    InsertSpace ->
      Just $ insertSpace gap
    InsertNewline ->
      Just $ insertNewline gap
    DoNothing ->
      Just gap

insertNewline :: Gap -> Gap
insertNewline (Gap oldLeft oldToken oldRight) =
  Gap (Tokens.snoc oldLeft Token.Newline) oldToken oldRight

insertSpace :: Gap -> Gap
insertSpace (Gap oldLeft oldToken oldRight) =
  Gap (Tokens.snoc oldLeft Token.Space) oldToken oldRight

delete :: Gap -> Maybe Gap
delete (Gap oldLeft _ oldRight) =
  maybeMap2
    ( \newToken newRight ->
        Gap oldLeft newToken newRight
    )
    (Tokens.head_ oldRight)
    (Tokens.tail_ oldRight)

moveRight :: Gap -> Maybe Gap
moveRight (Gap oldLeft oldToken oldRight) =
  maybeMap2
    ( \newToken newRight ->
        Gap (Tokens.snoc oldLeft oldToken) newToken newRight
    )
    (Tokens.head_ oldRight)
    (Tokens.tail_ oldRight)

moveLeft :: Gap -> Maybe Gap
moveLeft (Gap oldLeft oldToken oldRight) =
  maybeMap2
    ( \newLeft newToken ->
        Gap newLeft newToken (Tokens.cons oldToken oldRight)
    )
    (Tokens.init_ oldLeft)
    (Tokens.last_ oldLeft)
