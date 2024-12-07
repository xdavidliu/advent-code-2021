package main

import (
	"fmt"
	"math"
)

func main() {
	grid := readGrid("/home/xdavidliu/Documents/aoc/input12.txt")
	starts := findIndex('a', grid)
	dists := []int{bfs(grid, findIndex('S', grid)[0])}
	fmt.Println("part 1 =", dists[0]) // 361
	for _, start := range starts {
		dists = append(dists, bfs(grid, start))
	}
	fmt.Println("part 2 =", minimum(dists)) // 354
}

type position struct {
	row int
	col int
}

func findIndex(ch byte, grid *[][]byte) []position {
	var out []position
	for r, row := range *grid {
		for c, elem := range row {
			if elem == ch {
				out = append(out, position{r, c})
			}
		}
	}
	return out
}

func height(ch byte) int {
	switch ch {
	case 'S':
		return 0
	case 'E':
		return 25
	default:
		return int(ch) - int('a')
	}
}

type element struct {
	row  int
	col  int
	dist int
}

func bfs(grid *[][]byte, start position) int {
	nr := len(*grid)
	nc := len((*grid)[0])
	q := queue[element]{nil, nil}
	q.add(element{start.row, start.col, 0})
	seen := set[position]{}
	seen.insert(start)
	drs := []int{0, 0, 1, -1}
	dcs := []int{1, -1, 0, 0}
	for !q.isEmpty() {
		elem := q.remove()
		r, c, dist := elem.row, elem.col, elem.dist
		v := (*grid)[r][c]
		for k := range drs {
			rr, cc := r+drs[k], c+dcs[k]
			if rr < 0 || rr >= nr || cc < 0 || cc >= nc {
				continue
			} else if seen.contains(position{rr, cc}) {
				continue
			} else {
				vv := (*grid)[rr][cc]
				if height(vv)-height(v) > 1 {
					continue
				} else if vv == 'E' {
					return 1 + dist
				} else {
					q.add(element{rr, cc, dist + 1})
					seen.insert(position{rr, cc})
				}
			}
		}
	}
	return math.MaxInt
}
