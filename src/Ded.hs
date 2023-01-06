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
  | TopLevelBind TopLevelBindMachine
  | MakeIndent MakeIndentMachine
  deriving (Eq, Show)

stateMachine :: StateMachine -> Token.Token -> (StateMachine, Gap.Action)
stateMachine state token =
  case (state, token) of
    (FindTrailingSpace, Token.Equals) ->
      (Scrolling, Gap.MoveRightTwice)
    (Finished, Token.Equals) ->
      (Failed, Gap.DoNothing)
    (Failed, Token.Equals) ->
      (Failed, Gap.DoNothing)
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
    (Scrolling, Token.Equals) ->
      (TopLevelBind FindNewlineBeforeBind, Gap.MoveLeft)
    (TopLevelBind machine, _) ->
      topLevelBind machine token
    (MakeIndent machine, _) ->
      makeIndentMachine machine token

data MakeIndentMachine
  = One
  | Two
  | Three
  | Four
  | RemoveExtraSpaces
  deriving (Eq, Show)

makeIndentMachine :: MakeIndentMachine -> Token.Token -> (StateMachine, Gap.Action)
makeIndentMachine machine token =
  case (machine, token) of
    (One, Token.Space) ->
      (MakeIndent RemoveExtraSpaces, Gap.MoveRight)
    (One, Token.Newline) ->
      (MakeIndent RemoveExtraSpaces, Gap.InsertSpace)
    (One, Token.Equals) ->
      (MakeIndent RemoveExtraSpaces, Gap.InsertSpace)
    (One, Token.Verbatim _) ->
      (MakeIndent RemoveExtraSpaces, Gap.InsertSpace)
    (One, Token.Start) ->
      (Failed, Gap.DoNothing)
    (One, Token.End) ->
      (Failed, Gap.DoNothing)
    (Two, Token.Newline) ->
      (MakeIndent One, Gap.InsertSpace)
    (Two, Token.Equals) ->
      (MakeIndent One, Gap.InsertSpace)
    (Two, Token.Verbatim _) ->
      (MakeIndent One, Gap.InsertSpace)
    (Two, Token.Start) ->
      (Failed, Gap.DoNothing)
    (Two, Token.End) ->
      (Failed, Gap.DoNothing)
    (Three, Token.Newline) ->
      (MakeIndent Two, Gap.InsertSpace)
    (Three, Token.Equals) ->
      (MakeIndent Two, Gap.InsertSpace)
    (Three, Token.Verbatim _) ->
      (MakeIndent Two, Gap.InsertSpace)
    (Three, Token.Start) ->
      (MakeIndent Two, Gap.InsertSpace)
    (Three, Token.End) ->
      (Failed, Gap.DoNothing)
    (Four, Token.Newline) ->
      (MakeIndent Three, Gap.InsertSpace)
    (Four, Token.Equals) ->
      (MakeIndent Three, Gap.InsertSpace)
    (Four, Token.Verbatim _) ->
      (MakeIndent Three, Gap.InsertSpace)
    (Four, Token.Start) ->
      (Failed, Gap.DoNothing)
    (Four, Token.End) ->
      (Failed, Gap.DoNothing)
    (Two, Token.Space) ->
      (MakeIndent One, Gap.MoveRight)
    (Three, Token.Space) ->
      (MakeIndent Two, Gap.MoveRight)
    (Four, Token.Space) ->
      (MakeIndent Three, Gap.MoveRight)
    (RemoveExtraSpaces, Token.Space) ->
      (MakeIndent RemoveExtraSpaces, Gap.Delete)
    (RemoveExtraSpaces, Token.Newline) ->
      (MakeIndent RemoveExtraSpaces, Gap.Delete)
    (RemoveExtraSpaces, Token.Equals) ->
      (Scrolling, Gap.DoNothing)
    (RemoveExtraSpaces, Token.Verbatim _) ->
      (Scrolling, Gap.DoNothing)
    (RemoveExtraSpaces, Token.Start) ->
      (Failed, Gap.DoNothing)
    (RemoveExtraSpaces, Token.End) ->
      (Failed, Gap.DoNothing)

data TopLevelBindMachine
  = FindNewlineBeforeBind
  | ScrollPastEquals
  | StartOfBindLine
  | MoveToTopLevelBind
  | InsertNewlineAfterBind
  deriving (Eq, Show)

topLevelBind ::
  TopLevelBindMachine ->
  Token.Token ->
  (StateMachine, Gap.Action)
topLevelBind machine token =
  case (machine, token) of
    (FindNewlineBeforeBind, Token.OpenBracket) ->
        (TopLevelBind FindNewlineBeforeBind, Gap.MoveLeft)
    (FindNewlineBeforeBind, Token.CloseBracket) ->
        (TopLevelBind FindNewlineBeforeBind, Gap.MoveLeft)
    (StartOfBindLine, Token.OpenBracket) ->
        (TopLevelBind MoveToTopLevelBind, Gap.DoNothing)
    (StartOfBindLine, Token.CloseBracket) ->
        (TopLevelBind MoveToTopLevelBind, Gap.DoNothing)
    (ScrollPastEquals, Token.OpenBracket) ->
        (TopLevelBind ScrollPastEquals, Gap.MoveRight)
    (ScrollPastEquals, Token.CloseBracket) ->
        (TopLevelBind ScrollPastEquals, Gap.MoveRight)
    (MoveToTopLevelBind, Token.OpenBracket) ->
        (
    (FindNewlineBeforeBind, Token.Newline) ->
      (TopLevelBind StartOfBindLine, Gap.MoveRight)
    (StartOfBindLine, Token.Newline) ->
      (TopLevelBind ScrollPastEquals, Gap.MoveRight)
    (ScrollPastEquals, Token.Newline) ->
      (TopLevelBind ScrollPastEquals, Gap.MoveRight)
    (FindNewlineBeforeBind, Token.Space) ->
      (TopLevelBind FindNewlineBeforeBind, Gap.MoveLeft)
    (StartOfBindLine, Token.Space) ->
      (TopLevelBind ScrollPastEquals, Gap.MoveRight)
    (ScrollPastEquals, Token.Space) ->
      (TopLevelBind ScrollPastEquals, Gap.MoveRight)
    (FindNewlineBeforeBind, Token.Equals) ->
      (TopLevelBind ScrollPastEquals, Gap.MoveRight)
    (StartOfBindLine, Token.Equals) ->
      (TopLevelBind ScrollPastEquals, Gap.MoveRight)
    (ScrollPastEquals, Token.Equals) ->
      (TopLevelBind InsertNewlineAfterBind, Gap.MoveRight)
    (FindNewlineBeforeBind, Token.Verbatim _) ->
      (TopLevelBind FindNewlineBeforeBind, Gap.MoveLeft)
    (StartOfBindLine, Token.Verbatim _) ->
      (TopLevelBind MoveToTopLevelBind, Gap.MoveRight)
    (MoveToTopLevelBind, Token.Newline) ->
      (Failed, Gap.DoNothing)
    (ScrollPastEquals, Token.Verbatim _) ->
      (TopLevelBind ScrollPastEquals, Gap.DoNothing)
    (FindNewlineBeforeBind, Token.Start) ->
      (TopLevelBind MoveToTopLevelBind, Gap.MoveRight)
    (StartOfBindLine, Token.Start) ->
      (Failed, Gap.DoNothing)
    (MoveToTopLevelBind, Token.Space) ->
      (TopLevelBind MoveToTopLevelBind, Gap.MoveRight)
    (ScrollPastEquals, Token.Start) ->
      (Failed, Gap.DoNothing)
    (FindNewlineBeforeBind, Token.End) ->
      (Failed, Gap.DoNothing)
    (StartOfBindLine, Token.End) ->
      (Failed, Gap.DoNothing)
    (MoveToTopLevelBind, Token.Equals) ->
      (TopLevelBind InsertNewlineAfterBind, Gap.MoveRight)
    (InsertNewlineAfterBind, Token.Newline) ->
      (MakeIndent Four, Gap.MoveRight)
    (ScrollPastEquals, Token.End) ->
      (Failed, Gap.DoNothing)
    (MoveToTopLevelBind, Token.Verbatim _) ->
      (TopLevelBind MoveToTopLevelBind, Gap.MoveRight)
    (InsertNewlineAfterBind, Token.Space) ->
      (MakeIndent Four, Gap.InsertNewline)
    (InsertNewlineAfterBind, Token.Equals) ->
      (Failed, Gap.DoNothing)
    (MoveToTopLevelBind, Token.Start) ->
      (Failed, Gap.DoNothing)
    (InsertNewlineAfterBind, Token.Verbatim _) ->
      (TopLevelBind InsertNewlineAfterBind, Gap.InsertNewline)
    (MoveToTopLevelBind, Token.End) ->
      (Failed, Gap.DoNothing)
    (InsertNewlineAfterBind, Token.Start) ->
      (Failed, Gap.DoNothing)
    (InsertNewlineAfterBind, Token.End) ->
      (Failed, Gap.DoNothing)
