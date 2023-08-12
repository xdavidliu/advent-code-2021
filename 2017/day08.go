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
	fi, _ := os.Open("/home/xdavidliu/Documents/jetbrains-projects/goland/hello/data.txt")
	defer fi.Close()
	sc := bufio.NewScanner(bufio.NewReader(fi))
	// sc.Split(bufio.ScanLines)  // already the default
	for sc.Scan() {
		// works fine if final line is empty
		perform(sc.Text())
	}
	largestFinal := math.MinInt
	for _, v := range regVals {
		if v > largestFinal {
			largestFinal = v
		}
	}
	fmt.Println("part 1 =", largestFinal)
	fmt.Println("part 2 =", largestAtAnyTime)
}

func testCondition(cmp string, regVal int, condVal int) bool {
	switch cmp {
	case "<":
		return regVal < condVal
	case "<=":
		return regVal <= condVal
	case "==":
		return regVal == condVal
	case ">=":
		return regVal >= condVal
	case ">":
		return regVal > condVal
	case "!=":
		return regVal != condVal
	default:
		panic(nil)
	}
}

var regVals = map[string]int{}

var largestAtAnyTime = math.MinInt

func perform(s string) {
	words := strings.Split(s, " ")
	val, _ := strconv.Atoi(words[2])
	if words[1] == "dec" {
		val *= -1
	}
	condVal, _ := strconv.Atoi(words[6])
	condRegVal := regVals[words[4]] // 0 if not there is correct
	cmp := words[5]
	if testCondition(cmp, condRegVal, condVal) {
		reg := words[0]
		next := regVals[reg] + val
		regVals[reg] = next
		if next > largestAtAnyTime {
			largestAtAnyTime = next
		}
	}
}
