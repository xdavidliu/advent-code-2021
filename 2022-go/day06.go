package main

import (
	"fmt"
	"os"
)

func main() {
	filename := "/Users/xdavidliu/Documents/input06.txt"
	fin, _ := os.Open(filename)
	defer fin.Close()
	var line string
	fmt.Fscan(fin, &line)
	fmt.Printf("part 1 = %v\n", solve(line, 4))  // 1892
	fmt.Printf("part 1 = %v\n", solve(line, 14)) // 2313
}

func solve(line string, n int) int {
	s := []byte(line)
	m := make(map[byte]int)
	for i := 0; i < n; i++ {
		m[s[i]]++
	}
	if len(m) == n {
		return n
	}
	for i := n; i < len(s); i++ {
		m[s[i]]++
		p := m[s[i-n]]
		if p == 1 {
			delete(m, s[i-n])
		} else {
			m[s[i-n]]--
		}
		if len(m) == n {
			return i + 1
		}
	}
	panic("not found")
}
