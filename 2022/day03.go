package main

import (
	"bufio"
	"bytes"
	"os"
)

func findOnly(a []byte, b []byte) byte {
	for _, c := range b {
		if -1 != bytes.IndexByte(a, c) {
			return c
		}
	}
	return 0
}

func findOnlyThree(a []byte, b []byte, c []byte) byte {
	set := make(map[byte]bool)
	for _, x := range a {
		set[x] = false // true means in both a and b
	}
	for _, y := range b {
		if _, ok := set[y]; ok {
			set[y] = true
		}
	}
	for _, z := range c {
		if set[z] {
			return z
		}
	}
	panic("findOnlyThree")
}

func priority(x byte) int {
	if x >= 'a' {
		return 1 + int(x-'a')
	} else {
		return 27 + int(x-'A')
	}
}

func readLines(filename string) []string {
	fin, _ := os.Open(filename)
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	var lines []string
	for sc.Scan() {
		lines = append(lines, sc.Text())
	}
	return lines
}

func part1(lines []string) int {
	p1 := 0
	for _, t := range lines {
		n := len(t)
		h1 := t[0 : n/2]
		h2 := t[n/2 : n]
		c := findOnly([]byte(h1), []byte(h2))
		p1 += priority(c)
	}
	return p1
}

func part2(lines []string) int {
	p2 := 0
	for i := 0; i+2 < len(lines); i += 3 {
		a := []byte(lines[i])
		b := []byte(lines[i+1])
		c := []byte(lines[i+2])
		p2 += priority(findOnlyThree(a, b, c))
	}
	return p2
}

func main() {
	filename := "/home/xdavidliu/Documents/aoc/input03.txt"
	lines := readLines(filename)
	println("part 1 =", part1(lines)) // 7917
	println("part 2 =", part2(lines)) // 2585
}
