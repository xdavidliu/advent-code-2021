package main

import (
	"bufio"
	"fmt"
	"os"
)

type Cell = [2]int
type Inc = func(*[2]int)
type Check = func([2]int, *[][]byte) bool
type Compute = func(*[][]byte, Cell, Inc, Inc, Check, Check) *[][]int

func makeGrid(nr int, nc int) *[][]int {
	grid := make([][]int, nr)
	for i := range grid {
		grid[i] = make([]int, nc)
	}
	return &grid
}

func computeCanSee(
	grid *[][]byte, start Cell, outerInc Inc, innerInc Inc,
	outerCheck Check, innerCheck Check,
) *[][]int {
	canSee := makeGrid(len(*grid), len((*grid)[0]))
	for innerStart := start; outerCheck(innerStart, grid); outerInc(&innerStart) {
		var hi byte = 0
		for inner := innerStart; innerCheck(inner, grid); innerInc(&inner) {
			x := (*grid)[inner[0]][inner[1]]
			if x > hi {
				(*canSee)[inner[0]][inner[1]] = 1
				hi = x
			}
		}
	}
	return canSee
}

func goUp(ind *[2]int)    { ind[0]-- }
func goDown(ind *[2]int)  { ind[0]++ }
func goLeft(ind *[2]int)  { ind[1]-- }
func goRight(ind *[2]int) { ind[1]++ }

func insideLeft(ind [2]int, _ *[][]byte) bool     { return ind[1] >= 0 }
func insideRight(ind [2]int, grid *[][]byte) bool { return ind[1] < len((*grid)[0]) }
func insideAbove(ind [2]int, _ *[][]byte) bool    { return ind[0] >= 0 }
func insideBelow(ind [2]int, grid *[][]byte) bool { return ind[0] < len(*grid) }

func readGrid(filename string) *[][]byte {
	var grid [][]byte
	fin, _ := os.Open(filename)
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	for sc.Scan() {
		grid = append(grid, []byte(sc.Text()))
	}
	return &grid
}

func elemAt(grid *[][]byte, ind [2]int) byte {
	return (*grid)[ind[0]][ind[1]]
}

func computeNumberSeen(
	grid *[][]byte, start Cell, outerInc Inc, innerInc Inc,
	outerCheck Check, innerCheck Check,
) *[][]int {
	numSeen := makeGrid(len(*grid), len((*grid)[0]))
	for innerStart := start; outerCheck(innerStart, grid); outerInc(&innerStart) {
		// because cannot see any beyond edge
		(*numSeen)[innerStart[0]][innerStart[1]] = 0
		// because there's a tree there that will always be seen
		stack := [][2]int{innerStart}
		oneAfterInnerStart := innerStart
		innerInc(&oneAfterInnerStart)
		for inner := oneAfterInnerStart; innerCheck(inner, grid); innerInc(&inner) {
			g := elemAt(grid, inner)
			for 1 < len(stack) && g > elemAt(grid, stack[len(stack)-1]) {
				stack = stack[:len(stack)-1]
			}
			top := stack[len(stack)-1]
			// one of these diffs is zero
			(*numSeen)[inner[0]][inner[1]] = abs(inner[0]-top[0]) + abs(inner[1]-top[1])
			stack = append(stack, inner)
		}
	}
	return numSeen
}

type Update = func(int, int, int, int, int) int

func generalSolve(grid *[][]byte, compute Compute, update Update) int {
	nr := len(*grid)
	nc := len((*grid)[0])
	fromLeft := compute(grid, [2]int{0, 0}, goDown, goRight, insideBelow, insideRight)
	fromRight := compute(grid, [2]int{0, nc - 1}, goDown, goLeft, insideBelow, insideLeft)
	fromAbove := compute(grid, [2]int{0, 0}, goRight, goDown, insideRight, insideBelow)
	fromBelow := compute(grid, [2]int{nr - 1, 0}, goRight, goUp, insideRight, insideAbove)

	p := 0
	for r := 0; r < nr; r++ {
		for c := 0; c < nc; c++ {
			p = update(p, (*fromLeft)[r][c], (*fromRight)[r][c], (*fromAbove)[r][c], (*fromBelow)[r][c])
		}
	}
	return p
}

func highScore(p int, a int, b int, c int, d int) int {
	score := a * b * c * d
	if score > p {
		return score
	} else {
		return p
	}
}

func part2(grid *[][]byte) int {
	return generalSolve(grid, computeNumberSeen, highScore)
}

func incrementIfAny(p int, a int, b int, c int, d int) int {
	if a == 1 || b == 1 || c == 1 || d == 1 {
		return p + 1
	} else {
		return p
	}
}

func part1(grid *[][]byte) int {
	return generalSolve(grid, computeCanSee, incrementIfAny)
}

func main() {
	grid := readGrid("/home/xdavidliu/Documents/aoc/input08.txt")
	fmt.Println("part 1 =", part1(grid)) // 1681
	fmt.Println("part 2 =", part2(grid)) // 201684
}
