package main

import (
	"bufio"
	"os"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

func fill(lines []string, stacks [][]byte) {
	for _, line := range lines {
		i := 1 // "[B..."
		k := 0
		for i < len(line) {
			ch := line[i]
			if ch != ' ' {
				stacks[k] = append(stacks[k], line[i])
			}
			k++
			i += 4 // "...S] [P..."
		}
	}
	for _, st := range stacks {
		slices.Reverse(st)
	}
}

func move(stacks [][]byte, count int, from int, to int, rev bool) {
	fromSt := stacks[from]
	for k := 0; k < count; k++ {
		var d int
		if rev {
			d = count - 1 - k
		} else {
			d = k
		}
		stacks[to] = append(stacks[to], fromSt[len(fromSt)-1-d])
	}
	stacks[from] = fromSt[:len(fromSt)-count]
}

func deepCopy(src [][]byte) [][]byte {
	dest := make([][]byte, len(src))
	for i, x := range src {
		dest[i] = make([]byte, len(x))
		copy(dest[i], x)
	}
	return dest
}

func main() {
	filename := "/home/xdavidliu/Documents/aoc/input05.txt"
	fin, _ := os.Open(filename)
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	var lines []string
	nStack := 0
	for sc.Scan() {
		t := sc.Text()
		if -1 == strings.Index(t, "[") {
			nStack = len(strings.Fields(t))
			break
		} else {
			lines = append(lines, t)
		}
	}
	stacks := make([][]byte, nStack)
	fill(lines, stacks)
	stacksPart2 := deepCopy(stacks)
	re := regexp.MustCompile(`move (\d+) from (\d+) to (\d+)`)
	for sc.Scan() {
		t := sc.Text()
		if len(t) > 0 {
			toks := re.FindStringSubmatch(t)
			var a [3]int
			for i := 0; i < 3; i++ {
				a[i], _ = strconv.Atoi(toks[i+1])
			}
			move(stacks, a[0], a[1]-1, a[2]-1, false)
			move(stacksPart2, a[0], a[1]-1, a[2]-1, true)
		}
	}
	var tops []byte
	var topsPart2 []byte
	for _, st := range stacks {
		tops = append(tops, st[len(st)-1])
	}
	for _, st := range stacksPart2 {
		topsPart2 = append(topsPart2, st[len(st)-1])
	}
	println("part 1 =", string(tops))      // WCZTHTMPS
	println("part 2 =", string(topsPart2)) // BLSGJSDTS
}
