package main

import (
	"testing"
)

func TestAbs(t *testing.T) {
	for _, test := range tests {
		var buf [BUF_SIZE]byte
		copy(buf[:], []byte(test.input))
		size, err := format(buf, len(test.input))
		if err != nil {
			t.Errorf("%s: %s", test.description, err)
		}

		if string(buf[:size]) != test.expected {
			t.Errorf(
				"%s:\nexpected: %s\ngot: %s",
				test.description,
				test.expected,
				string(buf[:size]))
		}
	}
}

type Test struct {
	description string
	input       string
	expected    string
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
