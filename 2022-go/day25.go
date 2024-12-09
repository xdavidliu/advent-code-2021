package main

import (
	"bufio"
	"fmt"
	"os"
)

var val = map[byte]int{
	'2': 2, '1': 1, '0': 0, '-': -1, '=': -2,
}

var backVal = map[int]byte{
	0: '0', 1: '1', 2: '2', 3: '=', 4: '-',
}

func decimal(s []byte) int {
	r := 0
	for _, b := range s {
		r = r*5 + val[b]
	}
	return r
}

func snafu(i int) string {
	var s []byte
	for i > 0 {
		m := i % 5
		i /= 5
		if m >= 3 {
			i++
		}
		s = append(s, backVal[m])
	}
	reverse(s)
	return string(s)
}

func reverse(s []byte) {
	for l, r := 0, len(s)-1; l < r; l, r = l+1, r-1 {
		s[l], s[r] = s[r], s[l]
	}
}

func chk(e error) {
	if e != nil {
		panic(e)
	}
}

func main() {
	dir, err := os.UserHomeDir()
	chk(err)
	fin, err := os.Open(dir + "/Desktop/data.txt")
	chk(err)
	defer func() {
		err = fin.Close()
		chk(err)
	}()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	sum := 0
	for sc.Scan() {
		sum += decimal(sc.Bytes())
	}
	fmt.Println("part 1 =", snafu(sum))
	fmt.Println("part 2 is a freebie, just click the button")
}
