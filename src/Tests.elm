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
            [ ( "literal float"
              , [ ( "positive"
                  , "12.3"
                  , Just (Float 12.3)
                  )
                , ( "zero"
                  , "0.0"
                  , Just (Float 0)
                  )
                , ( "negative zero"
                  , "-0.0"
                  , Just (Float (negate 0))
                  )
                , ( "negative float"
                  , "-4.2"
                  , Just (Float -4.2)
                  )
                , ( "Scientific notation"
                  , "5e2"
                  , Just (Float 500)
                  )
                , ( "Scientific notation with dot"
                  , "5.12e2"
                  , Just (Float 512)
                  )
                , ( "Uppercase scientific notation"
                  , "5.12E2"
                  , Just (Float 512)
                  )
                , ( "Negative scientific notation"
                  , "-5.12e2"
                  , Just (Float -512)
                  )
                , ( "Exponent with explicit plus sign"
                  , "5e+2"
                  , Just (Float 500)
                  )
                , ( "Uppercase E and exponent with explicit plus sign"
                  , "5E+2"
                  , Just (Float 500)
                  )
                , ( "Negative exponent"
                  , "5e-2"
                  , Just (Float 0.05)
                  )
                , ( "Zero - exhibit 1"
                  , "0.0e5"
                  , Just (Float 0)
                  )
                , ( "Zero - exhibit 2"
                  , "0e5"
                  , Nothing
                  )
                , ( "starting with dot disallowed"
                  , ".123"
                  , Nothing
                  )
                , ( "ending with dot disallowed"
                  , "123."
                  , {- TODO perhaps make another test suite especially for
                       `number` that also tests which cases return which errors
                    -}
                    Nothing
                  )
                ]
              )
            , ( "literal char"
              , [ ( "number"
                  , "'1'"
                  , Just (Char '1')
                  )
                , ( "space"
                  , "' '"
                  , Just (Char ' ')
                  )
                , ( "newline shouldn't work"
                  , "'\n'"
                  , Nothing
                  )
                , ( "letter lowercase"
                  , "'a'"
                  , Just (Char 'a')
                  )
                , ( "letter uppercase"
                  , "'A'"
                  , Just (Char 'A')
                  )

                -- https://github.com/elm/compiler/blob/dcbe51fa22879f83b5d94642e117440cb5249bb1/compiler/src/Parse/String.hs#L279-L285
                , ( "escape backslash"
                  , singleQuote "\\\\"
                  , Just (Char '\\')
                  )
                , ( "escape n"
                  , singleQuote "\\n"
                  , Just (Char '\n')
                  )
                , ( "escape r"
                  , singleQuote "\\r"
                  , Just (Char '\u{000D}')
                  )
                , ( "escape t"
                  , singleQuote "\\t"
                  , Just (Char '\t')
                  )
                , ( "double quote"
                  , singleQuote "\""
                  , Just (Char '"')
                    -- " (for vscode-elm bug)
                  )
                , ( "single quote"
                  , singleQuote "\\'"
                  , Just (Char '\'')
                  )
                , ( "emoji"
                  , singleQuote "😃"
                  , Just (Char '😃')
                  )
                , ( "escaped unicode code point"
                  , singleQuote "\\u{1F648}"
                  , Just (Char '🙈')
                  )
                ]
              )
            , ( "literal string"
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
