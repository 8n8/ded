module ParserTest exposing (expr)


expr : Test
expr =
    let
        runSection ( description, tests ) =
            describe description
                (List.map runTest tests)

        runTest ( description, input, output ) =
            test description <|
                \() ->
                    input
                        |> P.run Stage.Parse.Parser.expr
                        |> Result.map Frontend.unwrap
                        |> expectEqualParseResult input output
    in
    describe "Stage.Parse.Parser.expr"
        (List.map runSection
            [ ( "literal multiline string"
              , [ ( "two numbers"
                  , tripleQuote "42"
                  , Just (String "42")
                  )
                , ( "single quote"
                  , tripleQuote "'"
                  , Just (String "'")
                  )
                , ( "double quote"
                  , tripleQuote " \" "
                  , Just (String " \" ")
                  )
                , ( "escape backslash"
                  , tripleQuote "\\\\"
                  , Just (String "\\")
                  )
                ]
              )
            ]
        )
