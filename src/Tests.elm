module ParserTest exposing (expr)


expr : Test
expr =
    describe "Stage.Parse.Parser.expr"
        ( "literal multiline string"
        , [ ( "escape backslash"
            , tripleQuote "\\\\"
            )
          ]
        )
