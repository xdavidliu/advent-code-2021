package main

import (
	"fmt"
	"os"
	"slices"
	"sort"
	"strconv"
	"strings"
	"unicode"
)

type value struct {
	num    int
	list   []value
	isList bool
}

func intValue(i int) value {
	return value{i, nil, false}
}

func singletonValue(i int) *value {
	v := value{0, []value{intValue(i)}, true}
	return &v
}

func doublyNestedSingleton(i int) *value {
	sv := singletonValue(i)
	v := value{0, []value{*sv}, true}
	return &v
}

func indexOfDoublyNestedSingleton(vals *[]*value, k int) int {
	for i, v := range *vals {
		if !v.isList || len(v.list) != 1 {
			continue
		}
		if !v.list[0].isList || len(v.list[0].list) != 1 {
			continue
		}
		w := v.list[0].list[0]
		if !w.isList && w.num == k {
			return i
		}
	}
	panic("index not found")
}

func parse(bs *[]byte, pos int) (value, int) {
	if unicode.IsDigit(rune((*bs)[pos])) {
		return parseNumber(bs, pos)
	} else if (*bs)[pos] == '[' {
		return parseList(bs, pos)
	} else {
		panic("parse on invalid pos")
	}
}

func isCommaOrEndBracket(b byte) bool {
	return b == ',' || b == ']'
}

func getNumberEnd(bs *[]byte, pos int) int {
	off := slices.IndexFunc((*bs)[pos:], isCommaOrEndBracket)
	if off == -1 {
		return len(*bs)
	} else {
		return pos + off
	}
}

func parseNumber(bs *[]byte, pos int) (value, int) {
	end := getNumberEnd(bs, pos)
	i, _ := strconv.Atoi(string((*bs)[pos:end]))
	return intValue(i), end
}

func parseList(bs *[]byte, pos int) (value, int) {
	if (*bs)[pos+1] == ']' {
		return value{0, nil, true}, pos + 2
	}
	start := pos
	var vals []value
	for (*bs)[start] != ']' { // first [ then ,
		v, end := parse(bs, start+1)
		vals = append(vals, v)
		start = end
	}
	// just past end of value. Consistent with parseNumber.
	return value{0, vals, true}, start + 1
}

func compare(left *value, right *value) int {
	if !left.isList && !right.isList {
		return left.num - right.num
	} else if !left.isList {
		return compare(singletonValue(left.num), right)
	} else if !right.isList {
		return compare(left, singletonValue(right.num))
	} else {
		for i := 0; i < len(left.list) && i < len(right.list); i++ {
			res := compare(&left.list[i], &right.list[i])
			if res != 0 {
				return res
			}
		}
		return len(left.list) - len(right.list)
	}
}

func main() {
	filename := "/usr/local/google/home/xdavidliu/Documents/aoc/input13.txt"
	bytesRead, _ := os.ReadFile(filename)
	blocks := strings.Split(string(bytesRead), "\n\n")
	var pairs [][2]*value
	for _, block := range blocks {
		lines := strings.Split(block, "\n")
		leftBytes := []byte(lines[0])
		rightBytes := []byte(lines[1])
		left, _ := parse(&leftBytes, 0)
		right, _ := parse(&rightBytes, 0)
		pairs = append(pairs, [2]*value{&left, &right})
	}
	items := []*value{doublyNestedSingleton(2), doublyNestedSingleton(6)}
	p1 := 0
	for i, pair := range pairs {
		if compare(pair[0], pair[1]) < 0 {
			p1 += i + 1
		}
		items = append(items, pair[0])
		items = append(items, pair[1])
	}
	fmt.Println("part 1 =", p1) // 5905
	sort.Slice(items, func(i int, j int) bool {
		return 0 > compare(items[i], items[j])
	})
	p2 := (1 + indexOfDoublyNestedSingleton(&items, 2)) *
		(1 + indexOfDoublyNestedSingleton(&items, 6))
	fmt.Println("part 2 =", p2) // 21691
}
