literalNumber : Parser_ LocatedExpr
literalNumber =
    let
        parseLiteralNumber =
            number
                { int = Int
                , hex = HexInt
                , float = Float
                }

        negateLiteral toBeNegated =
            case toBeNegated of
                Int int ->
                    Int (negate int)

                HexInt int ->
                    HexInt (negate int)

                Float float ->
                    Float (negate float)

                _ ->
                    toBeNegated
    in
    P.oneOf
        [ P.succeed negateLiteral
            |. P.symbol (P.Token "-" ExpectingMinusSign)
            |= parseLiteralNumber
        , parseLiteralNumber
        ]
        |> P.inContext InNumber
        |> located


type StringType
    = {- ' -} CharString
    | {- " -} NormalString
    | {- """ -} MultilineString


canContinueChompingString : StringType -> Char -> Bool
canContinueChompingString stringType char =
    case stringType of
        CharString ->
            char /= '\''

        NormalString ->
            char /= '"' && char /= '\\'

        MultilineString ->
            -- we'll have to check for the other two double-quotes afterwards
            char /= '"' && char /= '\\'


areChompedCharsOk : StringType -> String -> Bool
areChompedCharsOk stringType string =
    case stringType of
        CharString ->
            {- We could also check for the string only being 1 character long
               but we need to convert from String to Char anyway later so it's
               better done there. See `singleCharacter`.
            -}
            not <| String.contains "\n" string

        NormalString ->
            not <| String.contains "\n" string

        MultilineString ->
            True


apostrophe_ : P.Token ParseProblem
apostrophe_ =
    P.Token "'" ExpectingApostrophe


doubleQuote_ : P.Token ParseProblem
doubleQuote_ =
    P.Token "\"" ExpectingDoubleQuote


apostrophe : Parser_ ()
apostrophe =
    P.symbol apostrophe_


doubleQuote : Parser_ ()
doubleQuote =
    P.symbol doubleQuote_


singleCharacter : StringType -> Parser_ Char
singleCharacter stringType =
    stringContents stringType
        |> P.andThen
            (\chars ->
                case String.toList chars of
                    [ char ] ->
                        P.succeed char

                    _ ->
                        P.problem MoreThanOneCharInApostrophes
            )


backslash : Parser_ ()
backslash =
    P.token (P.Token "\\" ExpectingBackslash)
