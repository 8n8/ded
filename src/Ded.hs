module Ded (format) where

import qualified Data.ByteString
import qualified Gap
import qualified Token

format ::
  Data.ByteString.ByteString ->
  Either String Data.ByteString.ByteString
format unformatted =
  case Gap.init_ unformatted of
    Left err ->
      Left err
    Right gap ->
      case formatHelp Scrolling gap of
        Left err ->
          Left err
        Right formatted ->
          Right $ Gap.encode formatted

invalidElmFile :: String
invalidElmFile =
  "invalid Elm file"

formatHelp ::
  StateMachine ->
  Gap.Gap ->
  Either String Gap.Gap
formatHelp oldModel oldGap =
  let (newModel, action) = stateMachine oldModel (Gap.read_ oldGap)
   in if newModel == Finished
        then Right oldGap
        else case Gap.update action oldGap of
          Nothing ->
            Left invalidElmFile
          Just newGap ->
            formatHelp newModel newGap

data StateMachine
  = Scrolling
  | Failed
  | Finished
  | FindTrailingSpace
  deriving (Eq, Show)

stateMachine :: StateMachine -> Token.Token -> (StateMachine, Gap.Action)
stateMachine state token =
  case (state, token) of
    (Scrolling, Token.Newline) ->
      (FindTrailingSpace, Gap.MoveLeft)
    (Failed, Token.Newline) ->
      (Failed, Gap.DoNothing)
    (Finished, Token.Newline) ->
      (Finished, Gap.DoNothing)
    (Scrolling, Token.Space) ->
      (Scrolling, Gap.MoveRight)
    (Failed, Token.Space) ->
      (Failed, Gap.DoNothing)
    (Finished, Token.Space) ->
      (Finished, Gap.DoNothing)
    (Scrolling, Token.Verbatim _) ->
      (Scrolling, Gap.MoveRight)
    (Failed, Token.Verbatim _) ->
      (Failed, Gap.DoNothing)
    (Finished, Token.Verbatim _) ->
      (Finished, Gap.DoNothing)
    (Scrolling, Token.Start) ->
      (Scrolling, Gap.MoveRight)
    (Failed, Token.Start) ->
      (Failed, Gap.DoNothing)
    (Finished, Token.Start) ->
      (Finished, Gap.DoNothing)
    (Scrolling, Token.End) ->
      (Finished, Gap.DoNothing)
    (Failed, Token.End) ->
      (Failed, Gap.DoNothing)
    (Finished, Token.End) ->
      (Finished, Gap.DoNothing)
    (FindTrailingSpace, Token.Newline) ->
      (Scrolling, Gap.MoveRightTwice)
    (FindTrailingSpace, Token.Space) ->
      (Scrolling, Gap.Delete)
    (FindTrailingSpace, Token.Verbatim _) ->
      (Scrolling, Gap.MoveRightTwice)
    (FindTrailingSpace, Token.Start) ->
      (Scrolling, Gap.MoveRightTwice)
    (FindTrailingSpace, Token.End) ->
      (Finished, Gap.DoNothing)
