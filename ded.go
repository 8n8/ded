package main

const BUF_SIZE = 10 * 1000 * 1000

func format(buf *[BUF_SIZE]byte, size int) (int, error) {
	var result string = `module X exposing (x)


x =
    2
`
	copy((*buf)[:], []byte(result))
	return len(result), nil
}
