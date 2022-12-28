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
