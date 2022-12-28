{-|

     type X = Foo | Bar
     type X a = Foo a | Bar String

More generally,

     type <UserDefinedType> <VarType>* = <Constructor>[ | <Constructor>]*

     Constructor := <Name>[ <Type>]*

-}
customTypeDeclaration : Parser_ ( String, DeclarationBody LocatedExpr TypeAnnotation PossiblyQualified )
customTypeDeclaration =
    P.succeed
        (\name parameters constructors_ ->
            ( name
            , Declaration.CustomType
                { parameters = parameters
                , constructors = constructors_
                }
            )
        )
        |. P.keyword (P.Token "type" ExpectingTypeAlias)
        |. ignorables
        |= uppercaseNameWithoutDots
        |. P.oneOf
            [ P.symbol (P.Token " " ExpectingSpace)
            , P.symbol (P.Token "\n" ExpectingSpace)
            ]
        |. ignorables
        |= zeroOrMoreWith ignorables varName
        |. ignorables
        |. equals
        |. ignorables
        |= notAtBeginningOfLine constructors
        |> P.inContext InCustomType


constructors : Parser_ (NonEmpty (Constructor PossiblyQualified))
constructors =
    let
        subsequentConstructorsLoop reversedCtors =
            P.succeed (\x -> x)
                |. ignorables
                |= P.oneOf
                    [ P.succeed (\newCtor -> P.Loop (newCtor :: reversedCtors))
                        |. notAtBeginningOfLine (P.token (P.Token "|" ExpectingPipe))
                        |. ignorables
                        |= notAtBeginningOfLine constructor
                    , P.succeed (P.Done (List.reverse reversedCtors))
                    ]
    in
    P.succeed
        (\first rest ->
            List.NonEmpty.fromCons first rest
        )
        |= constructor
        |= P.loop [] subsequentConstructorsLoop
        |> P.inContext InConstructors


constructor : Parser_ (Constructor PossiblyQualified)
constructor =
    P.succeed Declaration.Constructor
        |= uppercaseNameWithoutDots
        |. ignorables
        |= oneOrMoreWith ignorables (notAtBeginningOfLine type_)


expr : Parser_ LocatedExpr
expr =
    PP.expression
        { oneOf =
            [ if_
            , let_
            , lambda
            , PP.literal literal
            , always varOrConstructorValue
            , list
            , parenStartingExpr
            , record
            , case_
            ]
        , andThenOneOf =
            -- TODO test this: does `x =\n  call 1\n+ something` work? (it shouldn't: no space before '+')
            [ PP.infixLeft 99
                (ignorablesAndCheckIndent (<) ExpectingIndentation)
                (Located.merge
                    (\fn argument ->
                        Frontend.Call
                            { fn = fn
                            , argument = argument
                            }
                    )
                )
            , PP.infixLeft 1
                (checkIndent (<) ExpectingIndentation
                    |> P.andThen (\() -> P.symbol (P.Token "++" ExpectingConcatOperator))
                )
                (Located.merge ListConcat)
            , PP.infixLeft 1
                (checkIndent (<) ExpectingIndentation
                    |> P.andThen (\() -> P.symbol (P.Token "+" ExpectingPlusOperator))
                )
                (Located.merge Plus)
            , PP.infixRight 1
                (checkIndent (<) ExpectingIndentation
                    |> P.andThen (\() -> P.symbol (P.Token "::" ExpectingConsOperator))
                )
                (Located.merge Cons)
            ]
        , spaces = ignorables
        }
        |> P.inContext InExpr


parenStartingExpr : ExprConfig -> Parser_ LocatedExpr
parenStartingExpr config =
    P.succeed identity
        |. leftParen
        |= P.oneOf
            [ P.succeed identity
                |= PP.subExpression 0 config
                |> P.andThen
                    (\e1 ->
                        P.oneOf
                            [ P.succeed identity
                                |. ignorables
                                |. comma
                                |= PP.subExpression 0 config
                                |> P.andThen
                                    (\e2 ->
                                        P.succeed identity
                                            |= P.oneOf
                                                [ -- ("x", "y", "z")
                                                  P.succeed (Frontend.Tuple3 e1 e2)
                                                    |. comma
                                                    |= PP.subExpression 0 config
                                                , -- ("x", "y")
                                                  P.succeed (Frontend.Tuple e1 e2)
                                                ]
                                    )
                            , -- ("x"), parenthesized expr
                              P.succeed (Located.unwrap e1)
                            ]
                    )
            , -- ()
              -- Note that unit can't be written as ( ) - no spaces inside!
              P.succeed Frontend.Unit
            ]
        |. rightParen
        |> located


leftParen : Parser_ ()
leftParen =
    P.symbol (P.Token "(" ExpectingLeftParen)


rightParen : Parser_ ()
rightParen =
    P.symbol (P.Token ")" ExpectingRightParen)


literal : Parser_ LocatedExpr
literal =
    P.oneOf
        [ literalNumber
        , literalChar
        , doubleQuoteStartingLiteral String
        , literalBool
        ]


type alias NumberConfig a =
    { int : Int -> a
    , hex : Int -> a
    , float : Float -> a
    }


number : NumberConfig a -> Parser_ a
number config =
    let
        finalizeFloat : { hasParsedDot : Bool, parsedIntPart : Int } -> Parser_ a
        finalizeFloat { hasParsedDot, parsedIntPart } =
            {- Float is just whatever the integer was plus `.` (if not parsed
               yet) and scientific notation (`e`)
            -}
            P.oneOf
                [ -- dot+decimal digits, ie. 123.5 or 123.5e8
                  P.succeed identity
                    |. (if hasParsedDot then
                            P.succeed ()

                        else
                            dot
                       )
                    |= P.getChompedString (P.chompWhile Char.isDigit)
                    |> P.andThen
                        (\chompedDecimalDigits ->
                            case String.toInt chompedDecimalDigits of
                                Just decimalDigits ->
                                    let
                                        decimalLength =
                                            String.length chompedDecimalDigits

                                        scaledDecimalDigits : Float
                                        scaledDecimalDigits =
                                            {- TODO would it be better to (A) scale the integer
                                               part up and adding non-scaled decimal part
                                               instead of (B) scaling decimal part down and adding
                                               to non-scaled integer part?

                                               Ie. for 123.45:

                                               (A) parsedIntPart = 123
                                                   decimalDigits = 45
                                                   scaledDecimalDigits = 0.45
                                                   intermediate result = 123.45
                                                   ... scientific notation scaling happens ...
                                                   result = ...

                                               (B) parsedIntPart = 123
                                                   decimalDigits = 45
                                                   scaledIntPart = 12300
                                                   intermediate result = 12345
                                                   ... scientific notation scaling happens ...
                                                   result = ...

                                               It seems like (B) would have less floating point
                                               problems?
                                            -}
                                            toFloat decimalDigits * 10 ^ negate (toFloat decimalLength)

                                        floatSoFar : Float
                                        floatSoFar =
                                            toFloat parsedIntPart + scaledDecimalDigits
                                    in
                                    P.oneOf
                                        [ scientificNotation floatSoFar
                                        , P.succeed (config.float floatSoFar)
                                        ]

                                Nothing ->
                                    -- This probably only happens on empty string
                                    P.problem FloatCannotEndWithDecimal
                        )
                , -- no dot+decimal digits, eg. 123e5
                  scientificNotation (toFloat parsedIntPart)
                , -- no dot+decimal digits, no `e`, eg. 123
                  P.succeed (config.int parsedIntPart)
                ]

        scientificNotationE : Parser_ ()
        scientificNotationE =
            P.chompIf (\c -> c == 'e' || c == 'E') ExpectingScientificNotationE

        scientificNotation : Float -> Parser_ a
        scientificNotation floatSoFar =
            P.succeed identity
                |. scientificNotationE
                |= P.oneOf
                    [ -- explicit '+' case
                      P.succeed identity
                        |. P.chompIf (\c -> c == '+') ExpectingScientificNotationPlus
                        |= P.getChompedString (P.chompWhile Char.isDigit)
                        |> P.andThen
                            (finalizeScientificNotation
                                { floatSoFar = floatSoFar
                                , shouldNegate = False
                                }
                            )
                    , -- explicit '-' case
                      P.succeed identity
                        |. P.chompIf (\c -> c == '-') ExpectingScientificNotationMinus
                        |= P.getChompedString (P.chompWhile Char.isDigit)
                        |> P.andThen
                            (finalizeScientificNotation
                                { floatSoFar = floatSoFar
                                , shouldNegate = True
                                }
                            )
                    , -- just a number
                      P.getChompedString (P.chompWhile Char.isDigit)
                        |> P.andThen
                            (finalizeScientificNotation
                                { floatSoFar = floatSoFar
                                , shouldNegate = False
                                }
                            )
                    ]

        finalizeScientificNotation : { floatSoFar : Float, shouldNegate : Bool } -> String -> Parser_ a
        finalizeScientificNotation { floatSoFar, shouldNegate } exponentDigits =
            case String.toInt exponentDigits of
                Nothing ->
                    P.problem ExpectingScientificNotationExponent

                Just exponent ->
                    let
                        floatExponent =
                            toFloat exponent

                        exponent_ =
                            if shouldNegate then
                                negate floatExponent

                            else
                                floatExponent
                    in
                    P.succeed (config.float (floatSoFar * 10 ^ exponent_))
    in
    P.oneOf
        [ P.succeed identity
            |. P.symbol (P.Token "0" ExpectingZero)
            |= P.oneOf
                [ scientificNotationE
                    |> P.andThen (\() -> P.problem IntZeroCannotHaveScientificNotation)
                , P.succeed identity
                    |. P.symbol (P.Token "x" ExpectingLowercaseX)
                    |= P.getChompedString (P.chompWhile Char.isHexDigit)
                    |> P.andThen
                        (\chompedHex ->
                            {- Usage of Hex.fromString saves us from checking
                               whether the string is empty.
                            -}
                            case Hex.fromString (String.toLower chompedHex) of
                                Err _ ->
                                    P.problem (ParseCompilerBug ParsedHexButCouldntConvert)

                                Ok int ->
                                    P.succeed (config.hex int)
                        )
                , P.succeed identity
                    |. dot
                    |= finalizeFloat
                        { hasParsedDot = True
                        , parsedIntPart = 0
                        }
                , P.succeed identity
                    |= P.getChompedString (P.chompWhile Char.isDigit)
                    |> P.andThen
                        (\chompedInt ->
                            if String.isEmpty chompedInt then
                                P.succeed (config.int 0)

                            else
                                P.problem IntCannotStartWithZero
                        )
                , -- TODO is this one needed?
                  P.succeed (config.int 0)
                ]
        , P.succeed identity
            |= P.getChompedString (P.chompWhile Char.isDigit)
            |> P.andThen
                (\chompedInt ->
                    case String.toInt chompedInt of
                        Nothing ->
                            if String.isEmpty chompedInt then
                                P.problem ExpectingNumber

                            else
                                P.problem (ParseCompilerBug ParsedIntButCouldntConvert)

                        Just int ->
                            finalizeFloat
                                { hasParsedDot = False
                                , parsedIntPart = int
                                }
                )
        ]


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
