package main

import (
	"testing"
)

func TestAbs(t *testing.T) {
	var buf [BUF_SIZE]byte
	for _, test := range tests {
		copy(buf[:], []byte(test.input))
		size, err := format(&buf, len(test.input))
		if err != nil {
			t.Errorf("%s: %s", test.description, err)
		}

		if string(buf[:size]) != test.expected {
			t.Errorf(
				"%s:\nexpected: %q\n     got: %q",
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
	Test{
		description: "Single trailing whitespace",
		input: `module X exposing (x) 


x =
    2
`,
		expected: `module X exposing (x)


x =
    2
`,
	},
	Test{
		description: "Double trailing whitespace",
		input: `module X exposing (x)  


x =
    2
`,
		expected: `module X exposing (x)


x =
    2
`,
	},
	Test{
		description: "Newline after top-level bind",
		input: `module X exposing (x)


x = 2
`,
		expected: `module X exposing (x)


x =
    2
`,
	},
	Test{
		description: "Formatted module with different name",
		input: `module Y exposing (y)


y =
    1
`,
		expected: `module Y exposing (y)


y =
    1
`,
	},
}
