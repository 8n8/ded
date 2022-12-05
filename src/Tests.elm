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
            [ ( "lambda"
              , [ ( "works with single argument"
                  , "\\x -> x + 1"
                  , Just
                        (Lambda
                            { arguments = [ "x" ]
                            , body =
                                Plus
                                    (Argument "x")
                                    (Int 1)
                            }
                        )
                  )
                , ( "multiline"
                  , """
                    \\x ->
                        x + 1
                    """
                        |> String.multilineInput
                  , Just
                        (Lambda
                            { arguments = [ "x" ]
                            , body =
                                Plus
                                    (Argument "x")
                                    (Int 1)
                            }
                        )
                  )
                , ( "works with multiple arguments"
                  , "\\x y -> x + y"
                  , Just
                        (Lambda
                            { arguments = [ "x", "y" ]
                            , body =
                                Plus
                                    (Argument "x")
                                    (Argument "y")
                            }
                        )
                  )
                ]
              )
            , ( "call"
              , [ ( "simple"
                  , "fn 1"
                  , Just
                        (Call
                            { fn = Var { name = "fn", qualifiedness = PossiblyQualified Nothing }
                            , argument = Int 1
                            }
                        )
                  )
                , ( "with var"
                  , "fn arg"
                  , Just
                        (Call
                            { fn = Var { name = "fn", qualifiedness = PossiblyQualified Nothing }
                            , argument = Var { name = "arg", qualifiedness = PossiblyQualified Nothing }
                            }
                        )
                  )
                , ( "multiple"
                  , "fn arg1 arg2"
                  , Just
                        (Call
                            { fn =
                                Call
                                    { fn = Var { name = "fn", qualifiedness = PossiblyQualified Nothing }
                                    , argument = Var { name = "arg1", qualifiedness = PossiblyQualified Nothing }
                                    }
                            , argument = Var { name = "arg2", qualifiedness = PossiblyQualified Nothing }
                            }
                        )
                  )
                , ( "space not needed if parenthesized arg"
                  , "fn(arg1)"
                  , Just
                        (Call
                            { fn = Var { name = "fn", qualifiedness = PossiblyQualified Nothing }
                            , argument = Var { name = "arg1", qualifiedness = PossiblyQualified Nothing }
                            }
                        )
                  )
                , ( "multiline"
                  , """
                    fn
                        arg1
                        arg2
                    """
                        |> String.multilineInput
                  , Just
                        (Call
                            { fn =
                                Call
                                    { fn = Var { name = "fn", qualifiedness = PossiblyQualified Nothing }
                                    , argument = Var { name = "arg1", qualifiedness = PossiblyQualified Nothing }
                                    }
                            , argument = Var { name = "arg2", qualifiedness = PossiblyQualified Nothing }
                            }
                        )
                  )
                ]
              )
            , ( "if"
              , [ ( "with one space"
                  , "if 1 then 2 else 3"
                  , Just
                        (If
                            { test = Int 1
                            , then_ = Int 2
                            , else_ = Int 3
                            }
                        )
                  )
                , ( "with multiple spaces"
                  , "if   1   then   2   else   3"
                  , Just
                        (If
                            { test = Int 1
                            , then_ = Int 2
                            , else_ = Int 3
                            }
                        )
                  )
                , ( "multiline"
                  , """
                    if 1 then
                        2

                    else
                        3

                    """
                        |> String.multilineInput
                  , Just
                        (If
                            { test = Int 1
                            , then_ = Int 2
                            , else_ = Int 3
                            }
                        )
                  )
                ]
              )
            , ( "literal int"
              , [ ( "positive"
                  , "123"
                  , Just (Int 123)
                  )
                , ( "zero"
                  , "0"
                  , Just (Int 0)
                  )
                , ( "negative zero"
                  , "-0"
                  , Just (Int (negate 0))
                  )
                , ( "hexadecimal int"
                  , "0x123abc"
                  , Just (HexInt 1194684)
                  )
                , ( "hexadecimal int - uppercase"
                  , "0x789DEF"
                  , Just (HexInt 7904751)
                  )
                , ( "hexadecimal int - mixed case"
                  , "0x789dEf"
                  , Just (HexInt 7904751)
                  )
                , ( "negative int"
                  , "-42"
                  , Just (Int -42)
                  )
                , ( "negative hexadecimal"
                  , "-0x123abc"
                  , Just (HexInt -1194684)
                  )
                , ( "starting with zero disallowed"
                  , "0123"
                  , Nothing
                  )
                , ( "e is interpreted as hex 14, not scientific notation"
                  , "0xABCe5"
                  , Just (HexInt 703717)
                  )
                ]
              )
            , ( "literal float"
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
            , ( "let"
              , [ ( "one liner"
                  , "let x = 1 in 2"
                  , Just
                        (Let
                            { bindings =
                                [ { name = "x"
                                  , body = Int 1
                                  }
                                ]
                            , body = Int 2
                            }
                        )
                  )
                , ( "one binding, generous whitespace"
                  , """
                    let
                      x =
                          1
                    in
                      2
                    """
                        |> String.multilineInput
                  , Just
                        (Let
                            { bindings =
                                [ { name = "x"
                                  , body = Int 1
                                  }
                                ]
                            , body = Int 2
                            }
                        )
                  )
                , ( "doesn't allow bindings on the same indentation level as `let`"
                  , """
                    let
                    x = 1
                    in
                      2
                    """
                        |> String.multilineInput
                  , Nothing
                  )
                , ( "allows result expr on the same indentation level as `let`"
                  , """
                    let
                     x = 1
                    in
                    2
                    """
                        |> String.multilineInput
                  , Just
                        (Let
                            { bindings =
                                [ { name = "x"
                                  , body = Int 1
                                  }
                                ]
                            , body = Int 2
                            }
                        )
                  )
                , ( "multiple bindings"
                  , """
                    let
                      x = 1
                      y = 2
                    in
                    3
                    """
                        |> String.multilineInput
                  , Just
                        (Let
                            { bindings =
                                [ { name = "x"
                                  , body = Int 1
                                  }
                                , { name = "y"
                                  , body = Int 2
                                  }
                                ]
                            , body = Int 3
                            }
                        )
                  )
                , ( "doesn't allow bindings to have different indentation from each other"
                  , """
                    let
                      x = 1
                       y = 2
                    in
                      3
                    """
                        |> String.multilineInput
                  , Nothing
                  )
                , ( "doesn't allow bindings to have different indentation from each other - the other way"
                  , """
                    let
                       x = 1
                      y = 2
                    in
                      3
                    """
                        |> String.multilineInput
                  , Nothing
                  )
                , ( "one binding that's used in the body"
                  , """
                    let
                      x = 2
                    in
                      1 + x
                    """
                        |> String.multilineInput
                  , Just
                        (Let
                            { bindings =
                                [ { name = "x"
                                  , body = Int 2
                                  }
                                ]
                            , body =
                                Plus
                                    (Int 1)
                                    (Argument "x")
                            }
                        )
                  )
                , ( "two bindings where one is dependent on the other"
                  , """
                    let
                      x = 2
                      y = x + 1
                    in
                      42
                    """
                        |> String.multilineInput
                  , Just
                        (Let
                            { bindings =
                                [ { name = "x"
                                  , body = Int 2
                                  }
                                , { name = "y"
                                  , body =
                                        Plus
                                            (Argument "x")
                                            (Int 1)
                                  }
                                ]
                            , body = Int 42
                            }
                        )
                  )
                ]
              )
            , ( "list"
              , [ ( "empty list"
                  , "[]"
                  , Just (List [])
                  )
                , ( "empty list with inner spaces"
                  , "[  ]"
                  , Just (List [])
                  )
                , ( "single item in list"
                  , "[1]"
                  , Just (List [ Int 1 ])
                  )
                , ( "single item in list with inner spaces"
                  , "[ 1 ]"
                  , Just (List [ Int 1 ])
                  )
                , ( "simple list"
                  , "[1,2,3]"
                  , Just
                        (List
                            [ Int 1
                            , Int 2
                            , Int 3
                            ]
                        )
                  )
                , ( "simple list with inner spaces"
                  , "[ 1,  2  , 3 ]"
                  , Just
                        (List
                            [ Int 1
                            , Int 2
                            , Int 3
                            ]
                        )
                  )
                , ( "list concat"
                  , "[] ++ []"
                  , Just (ListConcat (List []) (List []))
                  )
                , ( "multiline"
                  , """
                    [ 1
                    , 2
                    , 3
                    ]
                    """
                        |> String.multilineInput
                  , Just
                        (List
                            [ Int 1
                            , Int 2
                            , Int 3
                            ]
                        )
                  )
                ]
              )
            , ( "unit"
              , [ ( "simple case"
                  , "()"
                  , Just Unit
                  )
                ]
              )
            , ( "tuple"
              , [ ( "without spaces"
                  , "(1,2)"
                  , Just
                        (Tuple
                            (Int 1)
                            (Int 2)
                        )
                  )
                , ( "with inner spaces"
                  , "( 3 , 4 )"
                  , Just
                        (Tuple
                            (Int 3)
                            (Int 4)
                        )
                  )
                , ( "nested tuple"
                  , "(5,(6,7))"
                  , Just
                        (Tuple
                            (Int 5)
                            (Tuple (Int 6)
                                (Int 7)
                            )
                        )
                  )
                ]
              )
            , ( "tuple3"
              , [ ( "without spaces"
                  , "(1,2,3)"
                  , Just
                        (Tuple3
                            (Int 1)
                            (Int 2)
                            (Int 3)
                        )
                  )
                , ( "with inner spaces"
                  , "( 4 , 5 , 6 )"
                  , Just
                        (Tuple3
                            (Int 4)
                            (Int 5)
                            (Int 6)
                        )
                  )
                ]
              )
            , ( "cons"
              , [ ( "simple case"
                  , "1 :: []"
                  , Just
                        (Cons
                            (Int 1)
                            (List [])
                        )
                  )
                , ( "multiple values case"
                  , "1 :: 2 :: []"
                  , Just
                        (Cons
                            (Int 1)
                            (Cons
                                (Int 2)
                                (List [])
                            )
                        )
                  )
                , ( "no spaces"
                  , "1::[]"
                  , Just
                        (Cons
                            (Int 1)
                            (List [])
                        )
                  )
                , ( "multiple spaces"
                  , "1    ::      []"
                  , Just
                        (Cons
                            (Int 1)
                            (List [])
                        )
                  )
                ]
              )
            , ( "bin operator"
              , [ ( "combining binop with fn call - issue #75"
                  , "f a b c + 5"
                  , Just
                        (Plus
                            (Call
                                { fn =
                                    Call
                                        { fn =
                                            Call
                                                { fn = Var { name = "f", qualifiedness = PossiblyQualified Nothing }
                                                , argument = Var { name = "a", qualifiedness = PossiblyQualified Nothing }
                                                }
                                        , argument = Var { name = "b", qualifiedness = PossiblyQualified Nothing }
                                        }
                                , argument = Var { name = "c", qualifiedness = PossiblyQualified Nothing }
                                }
                            )
                            (Int 5)
                        )
                  )
                ]
              )
            , ( "record"
              , [ ( "empty record"
                  , "{}"
                  , Just (Record [])
                  )
                , ( "empty record with spaces"
                  , "{   }"
                  , Just (Record [])
                  )
                , ( "one field record"
                  , "{ a = 42 }"
                  , Just (Record [ { name = "a", body = Int 42 } ])
                  )
                , ( "one field record without spaces"
                  , "{a=42}"
                  , Just (Record [ { name = "a", body = Int 42 } ])
                  )
                , ( "two fields record"
                  , """{ a = 42, b = "hello" }"""
                  , Just
                        (Record
                            [ { name = "a", body = Int 42 }
                            , { name = "b", body = String "hello" }
                            ]
                        )
                  )
                , ( "multiline"
                  , """
                    { a = 42
                    , b = "hello" 
                    }
                    """
                  , Just
                        (Record
                            [ { name = "a", body = Int 42 }
                            , { name = "b", body = String "hello" }
                            ]
                        )
                  )
                ]
              )
            , ( "case"
              , [ ( "simple case"
                  , "case True of _->True"
                  , Just
                        (Case (Bool True)
                            [ { pattern = PAnything, body = Bool True }
                            ]
                        )
                  )
                , ( "multiline case"
                  , """
                    case 21 of
                        31 -> True
                        5 -> True
                        0xABC -> True
                        _ -> False
                    """
                        |> String.multilineInput
                  , Just
                        (Case (Int 21)
                            [ { pattern = PInt 31, body = Bool True }
                            , { pattern = PInt 5, body = Bool True }
                            , { pattern = PHexInt 2748, body = Bool True }
                            , { pattern = PAnything, body = Bool False }
                            ]
                        )
                  )
                , ( "complex case"
                  , """
                    case arg of
                        ('c', 23) ->
                            True
                        ("string") ->
                            True
                        ((arg1, arg2), 435.4) ->
                            False
                        [_, 45, (67.7)] ->
                            False
                        fst :: snd :: tail ->
                            False
                        ({ count } as alias1) as alias2 ->
                            False
                    """
                        |> String.multilineInput
                  , Just
                        (Case (Var { name = "arg", qualifiedness = PossiblyQualified Nothing })
                            [ { pattern = PTuple (PChar 'c') (PInt 23)
                              , body = Bool True
                              }
                            , { pattern = PString "string", body = Bool True }
                            , { pattern =
                                    PTuple
                                        (PTuple (PVar "arg1") (PVar "arg2"))
                                        (PFloat 435.4)
                              , body = Bool False
                              }
                            , { pattern = PList [ PAnything, PInt 45, PFloat 67.7 ]
                              , body = Bool False
                              }
                            , { pattern =
                                    PCons (PVar "fst")
                                        (PCons (PVar "snd") (PVar "tail"))
                              , body = Bool False
                              }
                            , { pattern =
                                    PAlias
                                        (PAlias (PRecord [ "count" ]) "alias1")
                                        "alias2"
                              , body = Bool False
                              }
                            ]
                        )
                  )
                ]
              )
            ]
        )
