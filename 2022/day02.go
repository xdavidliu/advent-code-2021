package main

import (
	"bufio"
	"fmt"
	"os"
)

func score(them int, me int) int {
	w := 0
	if them == me {
		w = 3
	} else if me == (them+1)%3 {
		w = 6
	}
	return me + 1 + w
}

func getMe(them int, me int) int {
	switch me {
	case 0:
		return (them + 2) % 3
	case 1:
		return them
	case 2:
		return (them + 1) % 3
	default:
		panic(me)
	}
}

func main() {
	filename := "/home/xdavidliu/Documents/aoc/input02.txt"
	fin, _ := os.Open(filename)
	defer fin.Close()
	nums := map[uint8]int{
		'A': 0,
		'B': 1,
		'C': 2,
		'X': 0,
		'Y': 1,
		'Z': 2,
	}
	sc := bufio.NewScanner(bufio.NewReader(fin))
	s1 := 0
	s2 := 0
	for sc.Scan() {
		t := sc.Text()
		them := nums[t[0]]
		me := nums[t[2]]
		s1 += score(them, me)
		s2 += score(them, getMe(them, me))
	}
	fmt.Println("part 1 =", s1) // 11449
	fmt.Println("part 2 =", s2) // 13187
}
