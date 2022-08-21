import main

minimal_with_extra_variable = {
    "elm.json": """{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0"
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.3"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
""",
    "src/Main.elm": """module Main exposing (main)

import Html

main =
    Html.text "hi"


a = 0
""",
}


minimal_without_extra_variable = {
    "src/Main.elm": """module Main exposing (main)

import Html

main =
    Html.text "hi"
"""
}


def test_simple():
    expected = minimal_without_extra_variable
    got = main.lint(minimal_with_extra_variable)
    assert expected == got
