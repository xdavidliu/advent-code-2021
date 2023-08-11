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
	inss := make([]instruction, 0, 1000) // capacity comes from # lines in data.txt
	for sc.Scan() {
		// works fine if final line is empty
		ins := parse(sc.Text())
		regVals[ins.reg] = 0
		inss = append(inss, ins)
	}
	largestAtAnyTime := math.MinInt
	for _, ins := range inss {
		if testCondition(ins) {
			next := regVals[ins.reg] + ins.val
			regVals[ins.reg] = next
			if next > largestAtAnyTime {
				largestAtAnyTime = next
			}
		}
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

var regVals = map[register]int{}

func testCondition(ins instruction) bool {
	regVal := regVals[ins.condReg]
	condVal := ins.condVal
	switch ins.cmp {
	case lessThan:
		return regVal < condVal
	case lessThanOrEqual:
		return regVal <= condVal
	case equal:
		return regVal == condVal
	case greaterThanOrEqual:
		return regVal >= condVal
	case greatThan:
		return regVal > condVal
	case notEqual:
		return regVal != condVal
	default:
		// recall that there are no enums in golang, so instruction
		// is a type alias, NOT an enum; hence no such thing as
		// "cover all the cases", thus this default is required.
		panic(nil)
	}
}

type register string

type instruction struct {
	reg     register
	val     int
	condReg register
	cmp     compare
	condVal int
}

type compare int

const (
	lessThan compare = iota
	lessThanOrEqual
	equal
	greaterThanOrEqual
	greatThan
	notEqual
)

var atocmp = map[string]compare{
	"<":  lessThan,
	"<=": lessThanOrEqual,
	"==": equal,
	">=": greaterThanOrEqual,
	">":  greatThan,
	"!=": notEqual,
}

// ugly but necessary to avoid "val: val" below
// https://stackoverflow.com/a/69189022/2990344
func atoi(s string) int {
	val, _ := strconv.Atoi(s)
	return val
}

func parse(s string) instruction {
	words := strings.Split(s, " ")
	val, _ := strconv.Atoi(words[2])
	if words[1] == "dec" {
		val *= -1
	}
	condVal, _ := strconv.Atoi(words[6])
	return instruction{
		reg:     register(words[0]),
		val:     val,
		condReg: register(words[4]), // 3 is "if"
		cmp:     atocmp[words[5]],
		condVal: condVal,
	}
}
