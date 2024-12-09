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

func nextRock() rock {
	i := theState.rockIdx
	theState.rockIdx = (i + 1) % len(rocks)
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

type state struct {
	rockIdx int
	dirIdx  int
}

var theState = state{0, 0}

func solve(fileName string, maxStopCount int) (states []state, highs []int) {
	states = make([]state, 0, maxStopCount)
	highs = make([]int, 0, maxStopCount)
	line := inputString(fileName)
	stopCount := 0
	// at beginning, lowest empty "cell" has y = 0, so
	// the ground can be thought of as a bunch of blocks
	// with y = -1.
	highest := -1
	for stopCount < maxStopCount {
		states = append(states, theState)
		highs = append(highs, highest)
		r := nextRock()
		// "left edge is two units away from the left wall and its bottom edge is
		// three units above the highest"
		// right above the highest rock is offset 0, i.e.
		// highest + 1, hence, "3 units above the highest rock" means
		// y = highest + 1 + 3
		off := point{2, highest + 1 + 3}
		// prevHighest := highest
		for {
			i := theState.dirIdx
			theState.dirIdx = (i + 1) % len(line)
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
	return
}

func firstPossibleCycle(states []state) (int, int) {
	inds := make(map[state][]int)
	for i, s := range states {
		inds[s] = append(inds[s], i)
	}
	for _, v := range inds {
		for l := 0; l+1 < len(v); l++ {
			// part after && says the second half can't go off edge
			for r := l + 1; r < len(v) && 2*v[r]-v[l] <= len(states); r++ {
				match := true
				for m := 1; v[l]+m < v[r]; m++ {
					if states[v[l]+m] != states[v[r]+m] {
						match = false
						break
					}
				}
				if match {
					// first match is also leftmost, and probably smallest
					return v[l], v[r]
				}
			}
		}
	}
	panic(nil)
}

func main() {
	directory := "/home/xdavidliu/Documents/jetbrains-projects/goland/hello"
	fileName := directory + "/data.txt"
	states, highs := solve(fileName, 10022)
	i, j := firstPossibleCycle(states) // found match 849 2594
	// if I increase maxStopCount higher than 10k, I get matches with v[r]-v[l] =
	// double that, so that's just four cycles instead of two. Hence, from above
	// we know that state at 849 and 2594 are beginnings of first and second cycles delta :
	// j - i = 1745
	const tenTo12 = 1000000000000
	quot := (tenTo12 - i) / (j - i)
	rem := (tenTo12 - i) % (j - i)
	diff := highs[i+rem] - highs[i]
	heightAtTenTo12 := highs[i] + quot*(highs[j]-highs[i]) + diff
	fmt.Println("part 2 =", heightAtTenTo12+1)
	//fmt.Println(highs[i])                // 1339
	//fmt.Println(highs[j] - highs[i])     // 2778
	//fmt.Println(highs[j+j-i] - highs[j]) // 2778
	// hence starting from i = 849, right before rock i falls (where first one is
	// rock 0), for every 1745 rocks that fall (including rock i), the height
	// increases by 2778 (starting from 1339 at i = 849)
	// hence, after 10^12 rocks have fallen, we are "right before rock 10^12"
	// hence (10^12 - 849) / 1745 = 573065902 rem 161
	// hence height at 10^12 is 1339 + 573065902 * 2778 + diff
	// where diff is h[849 + 161] - h[849]
	// fmt.Println(highs[849+161] - highs[849]) // 256
	// hence answer for part 2 is that + 1 = 1591977077352
}
