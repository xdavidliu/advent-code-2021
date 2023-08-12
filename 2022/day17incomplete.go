package main

import (
	"fmt"
	"os"
)

type point struct {
	x, y int
}
type rock []point

// want to set this to const, but golang doesn't have that
// https://stackoverflow.com/a/13140094/2990344
var rocks = [...]rock{
	{{0, 0}, {1, 0}, {2, 0}, {3, 0}},         // horizontal
	{{0, 1}, {1, 0}, {1, 1}, {1, 2}, {2, 1}}, // plus
	{{0, 0}, {1, 0}, {2, 0}, {2, 1}, {2, 2}}, // corner
	{{0, 0}, {0, 1}, {0, 2}, {0, 3}},         // vertical
	{{0, 0}, {0, 1}, {1, 0}, {1, 1}},         // square
}

var rockIdx = 0

func nextRock() rock {
	i := rockIdx
	rockIdx = (i + 1) % len(rocks)
	return rocks[i]
}

var filled = make(map[point]bool)

func leftFree(r rock, off point) bool {
	for _, p := range r {
		x := p.x + off.x
		y := p.y + off.y
		if x == 0 || filled[point{x - 1, y}] {
			return false
		}
	}
	return true
}

func rightFree(r rock, off point) bool {
	for _, p := range r {
		x := p.x + off.x
		y := p.y + off.y
		if x == 6 || filled[point{x + 1, y}] {
			return false
		}
	}
	return true
}

func downFree(r rock, off point) bool {
	for _, p := range r {
		x := p.x + off.x
		y := p.y + off.y
		if y == 0 || filled[point{x, y - 1}] {
			return false
		}
	}
	return true
}

func inputString(fileName string) string {
	fin, _ := os.Open(fileName) // "file in"
	defer fin.Close()
	var line string
	fmt.Fscan(fin, &line)
	return line
}

func solve(fileName string, maxStopCount int) {
	line := inputString(fileName)
	stopCount := 0
	dirIdx := 0 // direction index
	// at beginning, lowest empty "cell" has y = 0, so
	// the ground can be thought of as a bunch of blocks
	// with y = -1.
	highest := -1
	for stopCount < maxStopCount {
		r := nextRock()
		// "left edge is two units away from the left wall and its bottom edge is
		// three units above the highest"
		// right above the highest rock is offset 0, i.e.
		// highest + 1, hence, "3 units above the highest rock" means
		// y = highest + 1 + 3
		off := point{2, highest + 1 + 3}
		// prevHighest := highest
		for {
			i := dirIdx
			dirIdx = (i + 1) % len(line)
			switch line[i] {
			case '<':
				if leftFree(r, off) {
					off.x--
				}
			case '>':
				if rightFree(r, off) {
					off.x++
				}
			default:
				panic(nil)
			}
			if downFree(r, off) {
				off.y--
			} else {
				for _, p := range r {
					y := p.y + off.y
					if y > highest {
						highest = y
					}
					filled[point{p.x + off.x, y}] = true
				}
				stopCount++
				if stopCount == 2022 {
					fmt.Println("part 1 =", highest+1)
				}
				break // for {
			}
		} // for {
	} // for stopCount < maxStopCount {
}

func main() {
	directory := "/home/xdavidliu/Documents/jetbrains-projects/goland/hello"
	fileName := directory + "/data.txt"
	solve(fileName, 2022)
}
