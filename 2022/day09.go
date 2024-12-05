package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var dirx = map[uint8]int{
	'U': 0, 'D': 0, 'L': -1, 'R': 1,
}

var diry = map[uint8]int{
	'U': 1, 'D': -1, 'L': 0, 'R': 0,
}

func hmove(hx int, hy int, dir uint8) (int, int) {
	return hx + dirx[dir], hy + diry[dir]
}

func tmove(hx int, hy int, tx int, ty int) (int, int) {
	if abs(hx-tx) <= 1 && abs(hy-ty) <= 1 {
		return tx, ty
	} else if hx == tx {
		return tx, ty + sign(hy-ty)
	} else if hy == ty {
		return tx + sign(hx-tx), ty
	} else { // diagonal
		return tx + sign(hx-tx), ty + sign(hy-ty)
	}
}

func addToSet(seen *map[string]bool, a int, b int) {
	key := fmt.Sprintf("%d %d", a, b)
	(*seen)[key] = true
}

func main() {
	filename := "/home/xdavidliu/Documents/aoc/input09.txt"
	fin, _ := os.Open(filename)
	sc := bufio.NewScanner(bufio.NewReader(fin))
	seen1 := make(map[string]bool)
	seen2 := make(map[string]bool)
	addToSet(&seen1, 0, 0)
	addToSet(&seen2, 0, 0)
	x := make([]int, 10)
	y := make([]int, 10)
	for sc.Scan() {
		words := strings.Split(sc.Text(), " ")
		dir := []byte(words[0])[0]
		mag, _ := strconv.Atoi(words[1])
		for k := 0; k < mag; k++ {
			x[0], y[0] = hmove(x[0], y[0], dir)
			for i := 0; i < 9; i++ {
				x[i+1], y[i+1] = tmove(x[i], y[i], x[i+1], y[i+1])
			}
			addToSet(&seen1, x[1], y[1])
			addToSet(&seen2, x[len(x)-1], y[len(y)-1])
		}
	}
	fmt.Println("part 1 =", len(seen1)) // 5513
	fmt.Println("part 2 =", len(seen2)) // 2427
}
