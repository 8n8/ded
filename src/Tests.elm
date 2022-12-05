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
            [ ( "literal string"
              , [ ( "empty"
                  , doubleQuote ""
                  , Just (String "")
                  )
                , ( "one space"
                  , doubleQuote " "
                  , Just (String " ")
                  )
                , ( "newline shouldn't work"
                  , doubleQuote "\n"
                  , Nothing
                  )
                , ( "two numbers"
                  , doubleQuote "42"
                  , Just (String "42")
                  )
                , ( "single quote"
                  , doubleQuote "'"
                  , Just (String "'")
                  )
                , ( "double quote"
                  , doubleQuote "\\\""
                  , Just (String "\"")
                  )
                , ( "escape backslash"
                  , doubleQuote "\\\\"
                  , Just (String "\\")
                  )
                , ( "escape n"
                  , doubleQuote "\\n"
                  , Just (String "\n")
                  )
                , ( "escape r"
                  , doubleQuote "\\r"
                  , Just (String "\u{000D}")
                  )
                , ( "escape t"
                  , doubleQuote "\\t"
                  , Just (String "\t")
                  )
                , ( "emoji"
                  , doubleQuote "😃"
                  , Just (String "😃")
                  )
                , ( "escaped unicode code point"
                  , doubleQuote "\\u{1F648}"
                  , Just (String "🙈")
                  )
                , ( "combo of escapes and chars"
                  , doubleQuote "\\u{1F648}\\n\\r\\t\\\\abc123"
                  , Just (String "🙈\n\u{000D}\t\\abc123")
                  )
                ]
              )
            , ( "literal multiline string"
              , [ ( "empty"
                  , tripleQuote ""
                  , Just (String "")
                  )
                , ( "one space"
                  , tripleQuote " "
                  , Just (String " ")
                  )
                , ( "newline"
                  , tripleQuote "\n"
                  , Just (String "\n")
                  )
                , ( "two numbers"
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
                , ( "escape n"
                  , tripleQuote "\\n"
                  , Just (String "\n")
                  )
                , ( "escape r"
                  , tripleQuote "\\r"
                  , Just (String "\u{000D}")
                  )
                , ( "escape t"
                  , tripleQuote "\\t"
                  , Just (String "\t")
                  )
                , ( "emoji"
                  , tripleQuote "😃"
                  , Just (String "😃")
                  )
                , ( "escaped unicode code point"
                  , tripleQuote "\\u{1F648}"
                  , Just (String "🙈")
                  )
                , ( "combo of escapes, newlines, and chars"
                  , tripleQuote "\\u{1F648}\\n\n\n\\r\\t\\\\abc123"
                  , Just (String "🙈\n\n\n\u{000D}\t\\abc123")
                  )
                ]
              )
            , ( "literal bool"
              , [ ( "True"
                  , "True"
                  , Just (Bool True)
                  )
                , ( "False"
                  , "False"
                  , Just (Bool False)
                  )
                ]
              )
            ]
        )
