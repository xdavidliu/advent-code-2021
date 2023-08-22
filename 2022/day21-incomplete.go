package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type row struct {
	left  string
	mid   string
	op    byte
	right string
}

func valOrDefault(key string, vals map[string]int) string {
	val, ok := vals[key]
	if ok {
		return strconv.Itoa(val)
	} else {
		return key
	}
}

func (r row) show(vals map[string]int) string {
	return fmt.Sprintf(
		"%v = %v %v %v\n",
		valOrDefault(r.left, vals), valOrDefault(r.mid, vals),
		string([]byte{r.op}), valOrDefault(r.right, vals))
}

func assert(b bool) {
	if !b {
		panic(nil)
	}
}

func solve(r row, vals map[string]int) bool {
	vLeft, okLeft := vals[r.left]
	vMid, okMid := vals[r.mid]
	if !(okLeft || okMid) {
		return false
	}
	var ans int
	if okLeft && okMid {
		_, okRight := vals[r.right]
		assert(!okRight)
		switch r.op {
		case '+':
			ans = vLeft - vMid
		case '-':
			ans = vMid - vLeft
		case '*':
			ans = vLeft / vMid
		case '/':
			ans = vMid / vLeft
		default:
			panic(nil)
		}
		vals[r.right] = ans
		return true
	}
	assert(okMid != okLeft)
	vRight, okRight := vals[r.right]
	if !okRight {
		return false
	}
	if okLeft {
		switch r.op {
		case '+':
			ans = vLeft - vRight
		case '-':
			ans = vLeft + vRight
		case '*':
			ans = vLeft / vRight
		case '/':
			ans = vLeft * vRight
		default:
			panic(nil)
		}
		vals[r.mid] = ans
		return true
	} else {
		assert(okMid)
		switch r.op {
		case '+':
			ans = vMid + vRight
		case '-':
			ans = vMid - vRight
		case '*':
			ans = vMid * vRight
		case '/':
			ans = vMid / vRight
		default:
			panic(nil)
		}
		vals[r.left] = ans
		return true
	}
}

func solveAndReduce(rows []row, vals map[string]int) []row {
	c := 0
	for i, r := range rows {
		if !solve(r, vals) {
			if c != i {
				rows[c] = rows[i]
			}
			c++
			continue
		}
	}
	return rows[:c]
}

func part1(rows []row, vals map[string]int) {
	for len(rows) > 0 {
		rows = solveAndReduce(rows, vals)
	}
	fmt.Println("part 1 =", vals["root"])
}

func part2(rows []row, vals map[string]int, root row) {
	valRootMid, okRootMid := vals[root.mid]
	valRootRight, okRootRight := vals[root.right]
	for !(okRootRight && okRootMid) {
		rows = solveAndReduce(rows, vals)
		valRootMid, okRootMid = vals[root.mid]
		valRootRight, okRootRight = vals[root.right]
	}
	valHumn, okHumn := vals["humn"]
	assert(!okHumn)
	if okRootRight {
		vals[root.mid] = valRootRight
	} else {
		assert(okRootMid)
		vals[root.right] = valRootMid
	}
	fmt.Println("initially", len(rows))
	for !okHumn {
		before := len(rows)
		rows = solveAndReduce(rows, vals)
		if before == len(rows) {
			for _, r := range rows {
				fmt.Println(r.show(vals))
			}
			fmt.Println(before)
			// fmt.Println(vals)
			return
		}
		valHumn, okHumn = vals["humn"]
	}
	fmt.Println("part 2 =", valHumn)
}

func main() {
	fin, _ := os.Open("/home/xdavidliu/Desktop/data.txt")
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	vals := make(map[string]int)
	rows := make([]row, 0)
	for sc.Scan() {
		words := strings.Split(sc.Text(), " ")
		left := words[0]
		left = left[:len(left)-1] // omit colon :
		if len(words) == 2 {      // dbpl: 5
			v, err := strconv.Atoi(words[1])
			if err != nil {
				panic(nil)
			}
			vals[left] = v
		} else { // ptdq: humn - dvpt
			r := row{left: left, mid: words[1], op: words[2][0], right: words[3]}
			rows = append(rows, r)
		}
	}
	vals2 := make(map[string]int)
	rows2 := make([]row, len(rows))
	var root *row = nil
	c := 0
	for _, r := range rows {
		if r.left == "root" {
			root = &r
		} else {
			rows2[c] = r
			c++
		}
	}
	rows2 = rows2[:c]
	if root == nil {
		panic(nil)
	}
	for k, v := range vals {
		vals2[k] = v
	}
	delete(vals2, "humn") // in my data humn is a known number, not result of op
	part1(rows, vals)     // must be done AFTER vals2 constructed, since vals mutated
	part2(rows2, vals2, *root)
}
