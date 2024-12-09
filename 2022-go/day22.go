package main

import (
	"bufio"
	"fmt"
	"os"
)

func assert(b bool) {
	if !b {
		panic(nil)
	}
}

func read() ([][]byte, []byte) {
	// don't change this to example, since part2 only works for data
	fin, _ := os.Open("/home/xdavidliu/Desktop/data.txt")
	sc := bufio.NewScanner(bufio.NewReader(fin))
	rows := make([][]byte, 0)
	longest := 0
	for sc.Scan() {
		b := sc.Bytes()
		if len(b) == 0 {
			// empty line before last one
			break
		}
		r := make([]byte, len(b))
		if len(b) > longest {
			longest = len(b)
		}
		copy(r, b)
		rows = append(rows, r)
	}
	sc.Scan()
	b := sc.Bytes()
	path := make([]byte, len(b))
	copy(path, b)
	temp := make([][]byte, len(rows))
	copy(temp, rows)
	for i, r := range temp {
		assert(len(r) <= longest)
		if len(r) < longest {
			t := make([]byte, longest)
			copy(t, r)
			for k := len(r); k < longest; k++ {
				t[k] = ' '
			}
			rows[i] = t
		}
	}
	return rows, path
}

func limits(rows [][]byte) (leftCol []int, rightCol []int, upRow []int, downRow []int) {
	nrow, ncol := len(rows), len(rows[0])
	leftCol = make([]int, nrow)
	rightCol = make([]int, nrow)
	upRow = make([]int, ncol)
	downRow = make([]int, ncol)
	for r := range rows {
		var c int
		c = 0
		for rows[r][c] == ' ' {
			c++
		}
		leftCol[r] = c
		c = ncol - 1
		for rows[r][c] == ' ' {
			c--
		}
		rightCol[r] = c
	}
	for c := range rows[0] {
		var r int
		r = 0
		for rows[r][c] == ' ' {
			r++
		}
		upRow[c] = r
		r = nrow - 1
		for rows[r][c] == ' ' {
			r--
		}
		downRow[c] = r
	}
	return
}

var up = [2]int{-1, 0}
var down = [2]int{1, 0}
var left = [2]int{0, -1}
var right = [2]int{0, 1}

var facingValue = map[[2]int]int{
	right: 0,
	down:  1,
	left:  2,
	up:    3,
}

var rotateL = map[[2]int][2]int{
	right: up,
	up:    left,
	left:  down,
	down:  right,
}

var rotateR = map[[2]int][2]int{
	right: down,
	up:    right,
	left:  up,
	down:  left,
}

func part1(rows [][]byte, path []byte) {
	leftCol, rightCol, upRow, downRow := limits(rows)
	dir := [2]int{0, 1}          // facing to right
	pos := [2]int{0, leftCol[0]} // leftmost open tile of the top row
	mag := 0
	move := func() {
		for mag > 0 {
			next := [2]int{pos[0] + dir[0], pos[1] + dir[1]}
			if next[0] < upRow[pos[1]] {
				next[0] = downRow[pos[1]]
			} else if next[0] > downRow[pos[1]] {
				next[0] = upRow[pos[1]]
			}
			if next[1] < leftCol[pos[0]] {
				next[1] = rightCol[pos[0]]
			} else if next[1] > rightCol[pos[0]] {
				next[1] = leftCol[pos[0]]
			}
			if rows[next[0]][next[1]] == '#' {
				mag = 0
				break
			} else {
				pos[0] = next[0]
				pos[1] = next[1]
				mag--
			}
		}
	}
	for i := 0; i < len(path); i++ {
		switch path[i] {
		case 'L':
			move()
			dir = rotateL[dir]
		case 'R':
			move()
			dir = rotateR[dir]
		default:
			mag = mag*10 + int(path[i]-'0')
		}
	}
	move()
	part1 := 1000*(pos[0]+1) + 4*(pos[1]+1) + facingValue[dir]
	fmt.Println("part 1 =", part1)
}

// only works for my data
func part2(rows [][]byte, path []byte) {
	const n = 50
	dir := right
	pos := [2]int{0, n} // leftmost open tile of the top row
	mag := 0
	move := func() {
		/*
			     1    2

			     3

			4    5

			6

		*/
		for mag > 0 {
			r := pos[0] + dir[0]
			c := pos[1] + dir[1]
			newDir := dir
			if r == -1 && c < 2*n { // top of 1
				r = c + 2*n
				c = 0
				newDir = right
			} else if c == -1 && r >= 3*n { // left of 6
				c = r - 2*n
				r = 0
				newDir = down
			} else if r == -1 { // top of 2
				c -= 2 * n
				r = 4*n - 1
				newDir = up
			} else if r == 4*n { // bottom of 6
				c += 2 * n
				r = 0
				newDir = down
			} else if c == n-1 && r < n { // left of 1
				c = 0
				r = n - 1 - r + 2*n
				newDir = right
			} else if c == -1 { // left of 4
				c = n
				r = n - 1 - (r - 2*n)
				newDir = right
			} else if c == 3*n { // right of 2
				r = 2*n + n - 1 - r
				c = 2*n - 1
				newDir = left
			} else if c == 2*n && r >= 2*n { // right of 5
				r = n - 1 - (r - 2*n)
				c = 3*n - 1
				newDir = left
			} else if c == n-1 && r < 2*n && dir == left { // left of 3
				// dir check needed because r == 2 * n - 1 is also above 4
				c = r - n
				r = 2 * n
				newDir = down
			} else if r == 2*n-1 && c < n { // above 4
				r = c + n
				c = n
				newDir = right
			} else if r == n && c >= 2*n && dir == down { // below 2
				r = c - n
				c = 2*n - 1
				newDir = left
			} else if c == 2*n && r >= n { // right of 3
				c = r + n
				r = n - 1
				newDir = up
			} else if r == 3*n && c >= n && dir == down { // below 5
				r = c + 2*n
				c = n - 1
				newDir = left
			} else if c == n && r >= 3*n { // right of 6
				c = r - 2*n
				r = 3*n - 1
				newDir = up
			}
			switch rows[r][c] {
			case '#':
				mag = 0
				break
			case '.':
				pos[0] = r
				pos[1] = c
				mag--
				dir = newDir
			default:
				panic(nil)
			}
		}
	}
	for i := 0; i < len(path); i++ {
		switch path[i] {
		case 'L':
			move()
			dir = rotateL[dir]
		case 'R':
			move()
			dir = rotateR[dir]
		default:
			mag = mag*10 + int(path[i]-'0')
		}
	}
	move()
	ans := 1000*(pos[0]+1) + 4*(pos[1]+1) + facingValue[dir]
	fmt.Println("part 2 =", ans)
}

func main() {
	rows, path := read()
	part1(rows, path)
	part2(rows, path)
}
