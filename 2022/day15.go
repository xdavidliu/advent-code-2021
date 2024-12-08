package main

import (
	"bufio"
	"cmp"
	"fmt"
	"math"
	"os"
	"regexp"
	"slices"
	"strconv"
)

var sensorRe = regexp.MustCompile(
	`Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)`)

func parseLine(line string) [4]int {
	var out [4]int
	m := sensorRe.FindStringSubmatch(line)
	for i := 0; i < 4; i++ {
		out[i], _ = strconv.Atoi(m[i+1])
	}
	return out
}

func parseFile(filename string) *[][4]int {
	fin, _ := os.Open(filename)
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	var out [][4]int
	for sc.Scan() {
		out = append(out, parseLine(sc.Text()))
	}
	return &out
}

func dist(x0 int, y0 int, x1 int, y1 int) int {
	return abs(x0-x1) + abs(y0-y1)
}

func seg(y int, xs int, ys int, xb int, yb int) ([2]int, bool) {
	d := dist(xs, ys, xb, yb)
	dd := d - abs(ys-y)
	if dd <= 0 {
		return [2]int{0, 0}, false
	}
	return [2]int{xs - dd, xs + dd}, true
}

// apparently no need (*seg) here because [2] not []
func setClear(seg *[2]int) {
	seg[0] = math.MinInt
	seg[1] = math.MinInt
}

func isClear(seg [2]int) bool {
	return seg[0] == math.MinInt && seg[1] == math.MinInt
}

func comPairBackwards(a [2]int, b [2]int) int {
	c := cmp.Compare(a[1], b[1])
	if c != 0 {
		return c
	} else {
		return cmp.Compare(a[0], b[0])
	}
}

func lessEq(a int, b int, c int, d int) bool {
	return a <= b && b <= c && c <= d
}

// O(n^2); could be O(n log n) if we tried harder using more sophisticated
// interval algos
func makeDisjoint(segs *[][2]int) {
	slices.SortFunc(*segs, comPairBackwards)
	for i := range *segs {
		a := &(*segs)[i]
		if isClear(*a) {
			continue
		}
		for k := i + 1; k < len(*segs); k++ {
			b := &(*segs)[k]
			if isClear(*b) {
				continue
			}
			if lessEq(a[0], b[0], b[1], a[1]) {
				setClear(b)
			} else if lessEq(b[0], a[0], a[1], b[1]) {
				setClear(a)
				break
			} else if a[0]+1 <= b[0] && b[0] <= a[1]+1 {
				// a[1] = b[1]
				// b.clear()  // cannot do this because breaks sort invariant
				b[0] = a[0]
				setClear(a)
				break
			}
			// other case cannot be true since we sorted above
		}
	}
}

func computeDisjointSegs(tups *[][4]int, y int) *[][2]int {
	var segs [][2]int
	for _, tup := range *tups {
		s, ok := seg(y, tup[0], tup[1], tup[2], tup[3])
		if ok {
			segs = append(segs, s)
		}
	}
	makeDisjoint(&segs)
	out := slices.DeleteFunc(segs, isClear)
	return &out
}

func main() {
	filename := "/home/xdavidliu/Documents/aoc/input15.txt"
	tups := parseFile(filename)
	y := 2_000_000
	segs := computeDisjointSegs(tups, y)
	bys := make(set[int])
	for _, tup := range *tups {
		xb, yb := tup[2], tup[3]
		if y == yb {
			bys.insert(xb)
		}
	}
	nby := len(bys)
	total := 0
	for _, s := range *segs {
		total += 1 + s[1] - s[0]
	}
	fmt.Println("part 1 =", total-nby)        // 5394423
	fmt.Println("part 2 =", solvePart2(tups)) // 11840879211051
}

func solvePart2(tups *[][4]int) int {
	for y := 3_000_000; y < 4_000_005; y++ {
		segs := computeDisjointSegs(tups, y)
		if len(*segs) != 1 {
			xb := (*segs)[1][0] - 1
			return 4_000_000*xb + y
		}
	}
	panic("part 2")
}
