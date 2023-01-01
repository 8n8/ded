package main

import (
	"testing"
)

func TestAbs(t *testing.T) {
	for _, test := range tests {
		result, err := format([]byte(test.input))
		if err != nil {
			t.Errorf("%s: %s", test.description, err)
		}

		if string(result) != test.expected {
			t.Errorf("%s:\nexpected: %s\ngot: %s", test.description, test.expected, string(result))
		}
	}
}

type Test struct {
	description string
	input string
	expected string
}

var tests []Test = []Test{
	Test{
		description: "Hello world formatted, so don't change",
		input: `module X exposing (x)


x =
    2
`,
		expected: `module X exposing (x)


x =
    2
`,
	},
}
