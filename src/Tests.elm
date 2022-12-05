module ParserTest exposing (expr)


expr : Test
expr =
    describe "Stage.Parse.Parser.expr"
        [ ( "escape backslash"
          , tripleQuote "\\\\"
          )
        ]
