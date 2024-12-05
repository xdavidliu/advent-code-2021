package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func abs(x int) int {
	if x >= 0 {
		return x
	} else {
		return -x
	}
}

func update(during *int, ans1 *int, v int, crt *[]byte) {
	*during++
	pos := (*during - 1) % 40
	if 1 >= abs(pos-v) {
		(*crt)[*during-1] = '#'
	}
	if *during%40 == 20 {
		*ans1 += *during * v
	}
}

func dots(n int) *[]byte {
	a := make([]byte, n)
	for i := 0; i < n; i++ {
		a[i] = '.'
	}
	return &a
}

func main() {
	filename := "/home/xdavidliu/Documents/aoc/input10.txt"
	fin, _ := os.Open(filename)
	sc := bufio.NewScanner(bufio.NewReader(fin))
	during := 0
	v := 1
	ans1 := 0
	crt := dots(240)
	for sc.Scan() {
		t := sc.Text()
		update(&during, &ans1, v, crt)
		if t != "noop" {
			update(&during, &ans1, v, crt)
			toAdd, _ := strconv.Atoi(strings.Split(t, " ")[1])
			v += toAdd
		}
	}
	fmt.Println("part 1 =", ans1) // 15020
	for k := 0; k < 201; k += 40 {
		fmt.Println(string((*crt)[k : k+40]))
	} // EFUGLPAP
}
