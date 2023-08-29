package main

import (
	"bufio"
	"fmt"
	"os"
)

func chk(e error) {
	if e != nil {
		panic(e)
	}
}

func read() [][]byte {
	dir, err := os.UserHomeDir()
	chk(err)
	fin, err := os.Open(dir + "/Desktop/data.txt")
	chk(err)
	defer func() {
		err = fin.Close()
		chk(err)
	}()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	var gr [][]byte
	for sc.Scan() {
		b := sc.Bytes()
		c := make([]byte, len(b))
		copy(c, b)
		gr = append(gr, c)
	}
	return gr
}

const (
	dot   = '.'
	pound = '#'
	left  = '<'
	right = '>'
	up    = '^'
	down  = 'v'
)

func findDot(b []byte) int {
	for i, x := range b {
		if x == dot {
			return i
		}
	}
	panic(nil)
}

func checkData(gr [][]byte, startCol int, endCol int) {
	// make sure that first and last columns don't have any up or down
	// arrows; that way wind cannot get into the start or end positions,
	// and you don't have to treat those positions specially when
	// wrapping around
	cs := [2]int{startCol, endCol}
	for r := 1; r < len(gr)-1; r++ {
		for _, c := range cs {
			switch gr[r][c] {
			case up, down:
				panic(nil)
			default:
				continue
			}
		}
	}
}

type state struct {
	r, c, t, stage int
}

const (
	before = iota
	goalOnce
	startOnce
)

// have an extra one for staying put
var dr = [5]int{0, 0, 1, -1, 0}
var dc = [5]int{1, -1, 0, 0, 0}
var opposite = [4]byte{left, right, up, down}

func wallMod(i int, n int) int {
	if i < 0 {
		panic(nil)
	}
	return (i-1)%(n-2) + 1
}

var part1Done = false

func (s *state) explore(
	gr [][]byte, seen map[state]bool,
) (a []state, solved bool) {
	nr, nc := len(gr), len(gr[0])
	// 5 because staying put
	for i := 0; i < 5; i++ {
		sNext := state{s.r + dr[i], s.c + dc[i], s.t + 1, s.stage}
		if sNext.r == -1 || sNext.r == len(gr) || gr[sNext.r][sNext.c] == pound || seen[sNext] {
			// r == -1 only happens at start; and len(gr) only happens
			// at end for part 2
			continue
		}
		wind := false
		// 4 because looking for winds in the 4 directions
		// sNext.r not at top or bottom because can potentially wait
		// at start, and never any wind there. Also the wallMod
		// calculations don't work for r == 0 because it's
		// in the wall row.
		for k := 0; sNext.r != 0 && sNext.r != len(gr)-1 && k < 4; k++ {
			// add n to d to avoid negatives without changing
			// the wallMod: moving -1 is equivalent to moving
			// n then -1.
			rt1 := wallMod(sNext.r+(nr-2+dr[k])*sNext.t, nr)
			ct1 := wallMod(sNext.c+(nc-2+dc[k])*sNext.t, nc)
			if gr[rt1][ct1] == opposite[k] {
				wind = true
				break
			}
		}
		if sNext.r == 0 && gr[sNext.r][sNext.c] == dot && s.stage == goalOnce {
			sNext.stage = startOnce
		} else if sNext.r == len(gr)-1 && gr[sNext.r][sNext.c] == dot {
			switch s.stage {
			case before:
				if !part1Done {
					fmt.Println("part 1 =", sNext.t)
					part1Done = true
				}
				sNext.stage = goalOnce
				// don't return; append as usual
			case startOnce:
				solved = true
				a = []state{sNext}
				return
			}
		}
		if !wind {
			a = append(a, sNext)
			seen[sNext] = true
		}
	}
	return
}

type queue struct {
	data  []state
	front int
}

func (q *queue) add(s state) {
	q.data = append(q.data, s)
}

func (q *queue) remove() (s state, ok bool) {
	if q.front == len(q.data) {
		return // ok is false
	}
	ok = true
	s = q.data[q.front]
	q.front++
	if 2*q.front >= len(q.data) {
		for i := q.front; i < len(q.data); i++ {
			q.data[i-q.front] = q.data[i]
		}
		q.data = q.data[:len(q.data)-q.front]
		q.front = 0
	}
	return
}

func main() {
	gr := read()
	startCol := findDot(gr[0])
	endCol := findDot(gr[len(gr)-1])
	checkData(gr, startCol, endCol)
	q := queue{make([]state, 0), 0}
	start := state{0, startCol, 0, before}
	q.add(start)
	seen := make(map[state]bool)
	seen[start] = true
	for s, ok := q.remove(); ok; s, ok = q.remove() {
		a, solved := s.explore(gr, seen)
		if solved {
			fmt.Println("part 2 =", a[0].t)
			break
		}
		for _, s := range a {
			q.add(s)
			// no need to update seen; explore already does so
		}
	}
	// data.txt has dots of size 36 x 101, so even after 101 steps
	// it's not periodic
}
