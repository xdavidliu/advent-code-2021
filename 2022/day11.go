package main

import (
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

var primeProd int64 = 1

type monkey struct {
	q          queue[int64]
	op         func(int64) int64
	testVal    int
	count      int
	part       int
	trueOther  int
	falseOther int
}

func (m *monkey) work(monkeys *[]monkey) {
	for !m.q.isEmpty() {
		m.count++
		item := m.q.remove()
		item = m.op(item)
		switch m.part {
		case 1:
			item /= 3
		case 2:
			item %= primeProd
		}
		var other int
		if 0 == item%int64(m.testVal) {
			other = m.trueOther
		} else {
			other = m.falseOther
		}
		(*monkeys)[other].q.add(item)
	}
}

func add(x int64, y int64) int64 {
	return x + y
}

func mul(x int64, y int64) int64 {
	return x * y
}

func binaryOpFromStr(s string) func(int64, int64) int64 {
	switch s {
	case "+":
		return add
	case "*":
		return mul
	default:
		panic(s)
	}
}

func unaryOpFromString(s string) func(int64) int64 {
	operands := strings.Split(s, " ")
	binOp := binaryOpFromStr(operands[1])
	if operands[2] == "old" {
		return func(x int64) int64 {
			return binOp(x, x)
		}
	} else {
		c, _ := strconv.Atoi(operands[2])
		return func(x int64) int64 {
			return binOp(x, int64(c))
		}
	}
}

func fromBlock(block string) monkey {
	lines := strings.Split(block, "\n")
	items := strings.Split(strings.TrimPrefix(lines[1], "  Starting items: "), ", ")
	q := queue[int64]{nil, nil}
	for _, item := range items {
		x, _ := strconv.Atoi(item)
		q.add(int64(x))
	}
	opStr := strings.TrimPrefix(lines[2], "  Operation: new = ")
	op := unaryOpFromString(opStr)
	testStr := strings.TrimPrefix(lines[3], "  Test: divisible by ")
	testVal, _ := strconv.Atoi(testStr)
	trueStr := strings.TrimPrefix(lines[4], "    If true: throw to monkey ")
	trueOther, _ := strconv.Atoi(trueStr)
	falseStr := strings.TrimPrefix(lines[5], "    If false: throw to monkey ")
	falseOther, _ := strconv.Atoi(falseStr)
	return monkey{q, op, testVal, 0, 1, trueOther, falseOther}
}

func prodBestTwo(monkeys *[]monkey) int64 {
	best := math.MinInt
	second := math.MinInt
	for i := range *monkeys {
		c := (*monkeys)[i].count
		if c == best {
			second = c
		} else if c > best {
			second = best
			best = c
		} else if c > second {
			second = c
		}
	}
	return int64(best) * int64(second)
}

func main() {
	filename := "/home/xdavidliu/Documents/aoc/input11.txt"
	bytesRead, _ := os.ReadFile(filename)
	blocks := strings.Split(string(bytesRead), "\n\n")
	var monkeys1 []monkey = nil
	var monkeys2 []monkey = nil
	for _, block := range blocks {
		m1 := fromBlock(block)
		m2 := m1
		m2.q = m2.q.copy()
		m2.part = 2
		monkeys1 = append(monkeys1, m1)
		monkeys2 = append(monkeys2, m2)
	}
	for i := 0; i < 20; i++ {
		for k := range monkeys1 {
			monkeys1[k].work(&monkeys1)
		}
	}
	fmt.Println("part 1 =", prodBestTwo(&monkeys1)) // 107822
	for i := range monkeys2 {
		primeProd *= int64(monkeys2[i].testVal)
	}
	for i := 0; i < 10_000; i++ {
		for k := range monkeys2 {
			monkeys2[k].work(&monkeys2)
		}
	}
	fmt.Println("part 2 =", prodBestTwo(&monkeys2)) // 27267163742
}
