package main

import (
	"bufio"
	"fmt"
	"os"
	"sort"
	"strconv"
)

func topThree(filename string) []int {
	fin, _ := os.Open(filename)
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	sum := 0
	var nums []int
	for sc.Scan() {
		t := sc.Text()
		if len(t) > 0 {
			x, _ := strconv.Atoi(t)
			sum += x
		} else if sum != 0 {
			nums = append(nums, sum)
			sum = 0
		}
	}
	nums = append(nums, sum)
	// https://stackoverflow.com/a/18343326/2990344
	sort.Sort(sort.Reverse(sort.IntSlice(nums)))
	return nums[0:3]
}

func main() {
	t := topThree("/home/xdavidliu/Documents/aoc/input01.txt")
	fmt.Println("part 1 =", t[0])            // 66306
	fmt.Println("part 2 = ", t[0]+t[1]+t[2]) // 195292
}
