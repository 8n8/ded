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
  | MakeIndentOne
  | MakeIndentTwo
  | MakeIndentThree
  | MakeIndentFour
  | MakeIndentRemoveExtraSpaces
  | TopLevelBindFindNewlineBeforeBind
  | TopLevelBindScrollPastEquals
  | TopLevelBindStartOfBindLine
  | TopLevelBindMoveToTopLevelBind
  | TopLevelBindInsertNewlineAfterBind
  deriving (Eq, Show)

stateMachine :: StateMachine -> Token.Token -> (StateMachine, Gap.Action)
stateMachine state token =
  case (state, token) of
    (FindTrailingSpace, Token.Equals) ->
      (Scrolling, Gap.MoveRightTwice)
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
    (Finished, Token.Equals) ->
      (Failed, Gap.DoNothing)
    (Finished, Token.Newline) ->
      (Finished, Gap.DoNothing)
    (Finished, Token.Space) ->
      (Finished, Gap.DoNothing)
    (Finished, Token.Verbatim _) ->
      (Finished, Gap.DoNothing)
    (Finished, Token.Start) ->
      (Finished, Gap.DoNothing)
    (Finished, Token.End) ->
      (Finished, Gap.DoNothing)
    (Failed, Token.Equals) ->
      (Failed, Gap.DoNothing)
    (Failed, Token.Newline) ->
      (Failed, Gap.DoNothing)
    (Failed, Token.Space) ->
      (Failed, Gap.DoNothing)
    (Failed, Token.Verbatim _) ->
      (Failed, Gap.DoNothing)
    (Failed, Token.Start) ->
      (Failed, Gap.DoNothing)
    (Failed, Token.End) ->
      (Failed, Gap.DoNothing)
    (Scrolling, Token.Newline) ->
      (FindTrailingSpace, Gap.MoveLeft)
    (Scrolling, Token.Space) ->
      (Scrolling, Gap.MoveRight)
    (Scrolling, Token.Verbatim _) ->
      (Scrolling, Gap.MoveRight)
    (Scrolling, Token.Start) ->
      (Scrolling, Gap.MoveRight)
    (Scrolling, Token.End) ->
      (Finished, Gap.DoNothing)
    (Scrolling, Token.Equals) ->
      (TopLevelBindFindNewlineBeforeBind, Gap.MoveLeft)
    (MakeIndentOne, Token.Space) ->
      (MakeIndentRemoveExtraSpaces, Gap.MoveRight)
    (MakeIndentOne, Token.Newline) ->
      (MakeIndentRemoveExtraSpaces, Gap.InsertSpace)
    (MakeIndentOne, Token.Equals) ->
      (MakeIndentRemoveExtraSpaces, Gap.InsertSpace)
    (MakeIndentOne, Token.Verbatim _) ->
      (MakeIndentRemoveExtraSpaces, Gap.InsertSpace)
    (MakeIndentOne, Token.Start) ->
      (Failed, Gap.DoNothing)
    (MakeIndentOne, Token.End) ->
      (Failed, Gap.DoNothing)
    (MakeIndentTwo, Token.Newline) ->
      (MakeIndentOne, Gap.InsertSpace)
    (MakeIndentTwo, Token.Equals) ->
      (MakeIndentOne, Gap.InsertSpace)
    (MakeIndentTwo, Token.Verbatim _) ->
      (MakeIndentOne, Gap.InsertSpace)
    (MakeIndentTwo, Token.Start) ->
      (Failed, Gap.DoNothing)
    (MakeIndentTwo, Token.End) ->
      (Failed, Gap.DoNothing)
    (MakeIndentThree, Token.Newline) ->
      (MakeIndentTwo, Gap.InsertSpace)
    (MakeIndentThree, Token.Equals) ->
      (MakeIndentTwo, Gap.InsertSpace)
    (MakeIndentThree, Token.Verbatim _) ->
      (MakeIndentTwo, Gap.InsertSpace)
    (MakeIndentThree, Token.Start) ->
      (MakeIndentTwo, Gap.InsertSpace)
    (MakeIndentThree, Token.End) ->
      (Failed, Gap.DoNothing)
    (MakeIndentFour, Token.Newline) ->
      (MakeIndentThree, Gap.InsertSpace)
    (MakeIndentFour, Token.Equals) ->
      (MakeIndentThree, Gap.InsertSpace)
    (MakeIndentFour, Token.Verbatim _) ->
      (MakeIndentThree, Gap.InsertSpace)
    (MakeIndentFour, Token.Start) ->
      (Failed, Gap.DoNothing)
    (MakeIndentFour, Token.End) ->
      (Failed, Gap.DoNothing)
    (MakeIndentTwo, Token.Space) ->
      (MakeIndentOne, Gap.MoveRight)
    (MakeIndentThree, Token.Space) ->
      (MakeIndentTwo, Gap.MoveRight)
    (MakeIndentFour, Token.Space) ->
      (MakeIndentThree, Gap.MoveRight)
    (MakeIndentRemoveExtraSpaces, Token.Space) ->
      (MakeIndentRemoveExtraSpaces, Gap.Delete)
    (MakeIndentRemoveExtraSpaces, Token.Newline) ->
      (MakeIndentRemoveExtraSpaces, Gap.Delete)
    (MakeIndentRemoveExtraSpaces, Token.Equals) ->
      (Scrolling, Gap.DoNothing)
    (MakeIndentRemoveExtraSpaces, Token.Verbatim _) ->
      (Scrolling, Gap.DoNothing)
    (MakeIndentRemoveExtraSpaces, Token.Start) ->
      (Failed, Gap.DoNothing)
    (MakeIndentRemoveExtraSpaces, Token.End) ->
      (Failed, Gap.DoNothing)
    (TopLevelBindFindNewlineBeforeBind, Token.Newline) ->
      (TopLevelBindStartOfBindLine, Gap.MoveRight)
    (TopLevelBindFindNewlineBeforeBind, Token.Space) ->
      (TopLevelBindFindNewlineBeforeBind, Gap.MoveLeft)
    (TopLevelBindFindNewlineBeforeBind, Token.Equals) ->
      (TopLevelBindScrollPastEquals, Gap.MoveRight)
    (TopLevelBindFindNewlineBeforeBind, Token.Verbatim _) ->
      (TopLevelBindFindNewlineBeforeBind, Gap.MoveLeft)
    (TopLevelBindFindNewlineBeforeBind, Token.Start) ->
      (TopLevelBindMoveToTopLevelBind, Gap.MoveRight)
    (TopLevelBindFindNewlineBeforeBind, Token.End) ->
      (Failed, Gap.DoNothing)
    (TopLevelBindStartOfBindLine, Token.Newline) ->
      (TopLevelBindScrollPastEquals, Gap.MoveRight)
    (TopLevelBindStartOfBindLine, Token.Space) ->
      (TopLevelBindScrollPastEquals, Gap.MoveRight)
    (TopLevelBindStartOfBindLine, Token.Equals) ->
      (TopLevelBindScrollPastEquals, Gap.MoveRight)
    (TopLevelBindStartOfBindLine, Token.Verbatim _) ->
      (TopLevelBindMoveToTopLevelBind, Gap.MoveRight)
    (TopLevelBindStartOfBindLine, Token.Start) ->
      (Failed, Gap.DoNothing)
    (TopLevelBindStartOfBindLine, Token.End) ->
      (Failed, Gap.DoNothing)
    (TopLevelBindScrollPastEquals, Token.Newline) ->
      (TopLevelBindScrollPastEquals, Gap.MoveRight)
    (TopLevelBindScrollPastEquals, Token.Space) ->
      (TopLevelBindScrollPastEquals, Gap.MoveRight)
    (TopLevelBindScrollPastEquals, Token.Equals) ->
      (TopLevelBindInsertNewlineAfterBind, Gap.MoveRight)
    (TopLevelBindScrollPastEquals, Token.Verbatim _) ->
      (TopLevelBindScrollPastEquals, Gap.DoNothing)
    (TopLevelBindScrollPastEquals, Token.Start) ->
      (Failed, Gap.DoNothing)
    (TopLevelBindScrollPastEquals, Token.End) ->
      (Failed, Gap.DoNothing)
    (TopLevelBindMoveToTopLevelBind, Token.Newline) ->
      (Failed, Gap.DoNothing)
    (TopLevelBindMoveToTopLevelBind, Token.Space) ->
      (TopLevelBindMoveToTopLevelBind, Gap.MoveRight)
    (TopLevelBindMoveToTopLevelBind, Token.Equals) ->
      (TopLevelBindInsertNewlineAfterBind, Gap.MoveRight)
    (TopLevelBindMoveToTopLevelBind, Token.Verbatim _) ->
      (TopLevelBindMoveToTopLevelBind, Gap.MoveRight)
    (TopLevelBindMoveToTopLevelBind, Token.Start) ->
      (Failed, Gap.DoNothing)
    (TopLevelBindMoveToTopLevelBind, Token.End) ->
      (Failed, Gap.DoNothing)
    (TopLevelBindInsertNewlineAfterBind, Token.Newline) ->
      (MakeIndentFour, Gap.MoveRight)
    (TopLevelBindInsertNewlineAfterBind, Token.Space) ->
      (MakeIndentFour, Gap.InsertNewline)
    (TopLevelBindInsertNewlineAfterBind, Token.Equals) ->
      (Failed, Gap.DoNothing)
    (TopLevelBindInsertNewlineAfterBind, Token.Verbatim _) ->
      (TopLevelBindInsertNewlineAfterBind, Gap.InsertNewline)
    (TopLevelBindInsertNewlineAfterBind, Token.Start) ->
      (Failed, Gap.DoNothing)
    (TopLevelBindInsertNewlineAfterBind, Token.End) ->
      (Failed, Gap.DoNothing)
