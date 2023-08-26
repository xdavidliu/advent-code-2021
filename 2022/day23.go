package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
)

func chk(e error) {
	if e != nil {
		panic(e)
	}
}

type point struct {
	r int
	c int
}

func read() map[point]bool {
	dir, err := os.UserHomeDir()
	chk(err)
	fin, err := os.Open(dir + "/Desktop/data.txt")
	chk(err)
	defer func() {
		chk(fin.Close())
	}()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	elves := make(map[point]bool)
	r := 0
	for sc.Scan() {
		for c, b := range sc.Bytes() {
			if b == '#' {
				elves[point{r, c}] = true
			}
		}
		r++
	}
	return elves
}

func (p point) around() [8]point {
	return [8]point{
		{p.r - 1, p.c - 1}, {p.r - 1, p.c}, {p.r - 1, p.c + 1},
		{p.r, p.c - 1}, {p.r, p.c + 1},
		{p.r + 1, p.c - 1}, {p.r + 1, p.c}, {p.r + 1, p.c + 1},
	}
}

func (p point) north() [3]point {
	return [3]point{
		{p.r - 1, p.c - 1}, {p.r - 1, p.c}, {p.r - 1, p.c + 1},
	}
}

func (p point) south() [3]point {
	return [3]point{
		{p.r + 1, p.c - 1}, {p.r + 1, p.c}, {p.r + 1, p.c + 1},
	}
}

func (p point) west() [3]point {
	return [3]point{
		{p.r - 1, p.c - 1}, {p.r, p.c - 1}, {p.r + 1, p.c - 1},
	}
}

func (p point) east() [3]point {
	return [3]point{
		{p.r - 1, p.c + 1}, {p.r, p.c + 1}, {p.r + 1, p.c + 1},
	}
}

func empty(elves map[point]bool, ps []point) bool {
	for _, p := range ps {
		if elves[p] {
			return false
		}
	}
	return true
}

func propose(
	i int, p point, elves map[point]bool,
	dest map[point]point, dupe map[point]bool) bool {
	var ns [3]point
	var to point
	switch i {
	case 0:
		ns = p.north()
		to = point{p.r - 1, p.c}
	case 1:
		ns = p.south()
		to = point{p.r + 1, p.c}
	case 2:
		ns = p.west()
		to = point{p.r, p.c - 1}
	case 3:
		ns = p.east()
		to = point{p.r, p.c + 1}
	default:
		panic(i)
	}
	if !empty(elves, ns[:]) {
		return false
	}
	if _, ok := dest[to]; ok {
		dupe[to] = true
	} else {
		dest[to] = p
	}
	return true
}

func once(elves map[point]bool, round int) bool {
	// proposal from value to key
	dest := make(map[point]point)
	dupe := make(map[point]bool)
	for p := range elves {
		a := p.around()
		if empty(elves, a[:]) {
			continue
		}
		for i := 0; i < 4; i++ {
			if propose((round+i)%4, p, elves, dest, dupe) {
				break
			}
		}
	}
	changed := false
	for to, from := range dest {
		if !dupe[to] {
			delete(elves, from)
			elves[to] = true
			changed = true
		}
	}
	return changed
}

func area(elves map[point]bool) int {
	n, s, w, e := math.MaxInt, math.MinInt, math.MaxInt, math.MinInt
	for p := range elves {
		if p.r < n {
			n = p.r
		}
		if p.r > s {
			s = p.r
		}
		if p.c < w {
			w = p.c
		}
		if p.c > e {
			e = p.c
		}
	}
	return (s-n+1)*(e-w+1) - len(elves)
}

func main() {
	elves := read()
	area(elves)
	var k int
	for k = 0; ; k++ {
		if !once(elves, k) {
			break
		}
		if k == 10-1 {
			fmt.Println("part 1 =", area(elves))
		}
	}
	fmt.Println("part 2 =", k+1)
}
