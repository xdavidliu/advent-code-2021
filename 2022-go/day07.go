package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
)

type dir struct {
	name           string
	parent         *dir
	children       []*dir
	directFileSize int
	// children must be *dir not dir, because if latter, growth of slice
	// when appending will make copies, thus pwd pointers to original
	// will get invalidated.
}

func goToChildDir(parent *dir, childName string) *dir {
	for _, child := range parent.children {
		if childName == child.name {
			return child
		}
	}
	panic("child not found")
}

func collectSizes(d *dir, sizes *[]int) int {
	sz := d.directFileSize
	for _, c := range d.children {
		sz += collectSizes(c, sizes)
	}
	*sizes = append(*sizes, sz)
	return sz
}

func main() {
	filename := "/home/xdavidliu/Documents/aoc/input07.txt"
	fin, _ := os.Open(filename)
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	root := dir{"/", nil, nil, 0}
	pwd := &root
	cdPrefix := "$ cd "
	dirPrefix := "dir "
	for sc.Scan() {
		t := sc.Text()
		if t == "$ cd /" {
			pwd = &root
		} else if t == "$ cd .." {
			pwd = pwd.parent
		} else if strings.HasPrefix(t, cdPrefix) {
			// assumes ls already done
			pwd = goToChildDir(pwd, strings.TrimPrefix(t, cdPrefix))
		} else if strings.HasPrefix(t, dirPrefix) {
			// assumes every dir LSed only once
			child := &dir{
				strings.TrimPrefix(t, dirPrefix),
				pwd,
				nil,
				0,
			}
			pwd.children = append(pwd.children, child)
		} else if strings.HasPrefix(t, "$ ls ") { // no action needed
		} else {
			sz, _ := strconv.Atoi(strings.Split(t, " ")[0])
			pwd.directFileSize += sz
		}
	}
	var sizes []int
	collectSizes(&root, &sizes)
	part1 := 0
	// root total at end because last call in recursion
	actualFreeSpace := 70_000_000 - sizes[len(sizes)-1]
	moreSpaceNeeded := 30_000_000 - actualFreeSpace
	part2 := math.MaxInt
	for _, sz := range sizes {
		if sz <= 100_000 {
			part1 += sz
		}
		if sz > moreSpaceNeeded && sz < part2 {
			part2 = sz
		}
	}
	fmt.Println("part 1 =", part1) // 2061777
	fmt.Println("part 2 =", part2) // 4473403
}
