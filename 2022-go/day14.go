package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	filename := "/home/xdavidliu/Documents/aoc/input14.txt"
	paths := readPaths(filename)
	bottom := math.MinInt
	left := math.MaxInt
	right := math.MinInt
	for _, path := range paths {
		for _, point := range path {
			bottom = max(bottom, point[1])
			left = min(left, point[0])
			right = max(right, point[0])
		}
	}
	nrow := bottom + 1
	ncol := 1 + right - left
	grid := makeGrid[byte](nrow, ncol)
	fillGrid(grid, '.')
	writeHashPaths(grid, &paths, left)
	fmt.Println("part 1 =", solve(grid, left, right, bottom)) // 779
	left = 500 - bottom - 2
	right = 500 + bottom + 2
	bottom += 2
	nrow = bottom + 1
	ncol = 1 + right - left
	grid = makeGrid[byte](nrow, ncol)
	fillGrid(grid, '.')
	writeHashPaths(grid, &paths, left)
	for x := 0; x < ncol; x++ {
		(*grid)[bottom][x] = '#'
	}
	fmt.Println("part 2 =", solve(grid, left, right, bottom)) // 27426
}

func readPaths(filename string) [][][2]int {
	fin, _ := os.Open(filename)
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	var paths [][][2]int
	for sc.Scan() {
		paths = append(paths, makePath(sc.Text()))
	}
	return paths
}

func writeHashPaths(grid *[][]byte, paths *[][][2]int, left int) {
	for _, path := range *paths {
		for i := 1; i < len(path); i++ {
			x0 := path[i-1][0]
			y0 := path[i-1][1]
			x1 := path[i][0]
			y1 := path[i][1]
			writeHash(grid, left, x0, y0, x1, y1)
		}
	}
}

func makePath(ln string) [][2]int {
	points := strings.Split(ln, " -> ")
	var out [][2]int
	for _, p := range points {
		nums := strings.Split(p, ",")
		a, _ := strconv.Atoi(nums[0])
		b, _ := strconv.Atoi(nums[1])
		out = append(out, [2]int{a, b})
	}
	return out
}

func writeHash(grid *[][]byte, left int, x0 int, y0 int, x1 int, y1 int) {
	endX := max(x0, x1)
	endY := max(y0, y1)
	for x := min(x0, x1); x <= endX; x++ {
		for y := min(y0, y1); y <= endY; y++ {
			(*grid)[y][x-left] = '#'
		}
	}
}

func solve(grid *[][]byte, left int, right int, bottom int) int {
	count := 0
	for {
		x, y := 500, 0
		if (*grid)[y][x-left] == 'O' {
			return count // part 2
		}
		for {
			if y == bottom {
				return count
			} else if (*grid)[y+1][x-left] == '.' {
				y++
			} else if x == left {
				return count
			} else if (*grid)[y+1][x-left-1] == '.' {
				y++
				x--
			} else if x == right {
				return count
			} else if (*grid)[y+1][x-left+1] == '.' {
				y++
				x++
			} else {
				(*grid)[y][x-left] = 'O'
				count++
				break
			}
		}
	}
}
