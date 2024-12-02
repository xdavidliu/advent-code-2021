package main

import (
	"bufio"
	"os"
	"regexp"
	"strconv"
)

func contained(a int, b int, c int, d int) bool {
	return a <= c && b >= d || c <= a && d >= b
}

func disjoint(a int, b int, c int, d int) bool {
	return b < c || a > d
}

func main() {
	filename := "/home/xdavidliu/Documents/aoc/input04.txt"
	fin, _ := os.Open(filename)
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	re := regexp.MustCompile(`(\d+)-(\d+),(\d+)-(\d+)`)
	p1 := 0
	p2 := 0
	for sc.Scan() {
		words := re.FindStringSubmatch(sc.Text())
		var a [4]int
		for i := 0; i < 4; i++ {
			a[i], _ = strconv.Atoi(words[i+1])
		}
		if contained(a[0], a[1], a[2], a[3]) {
			p1++
		}
		if !disjoint(a[0], a[1], a[2], a[3]) {
			p2++
		}
	}
	println("part 1 =", p1) // 580
	println("part 2 =", p2)  // 895
}
