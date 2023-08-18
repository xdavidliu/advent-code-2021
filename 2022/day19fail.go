package main

import (
	"bufio"
	"fmt"
	"math/rand"
	"os"
	"strconv"
	"strings"
)

func solveOnce(costs robotCosts) int {
	var ore, clay, obsidian, geode int
	var oreRobot, clayRobot, obsidianRobot, geodeRobot int
	oreRobot = 1
	for t := 0; t < 24; t++ {
		var deltaOreRobot, deltaClayRobot, deltaObsidianRobot, deltaGeodeRobot int
		if ore >= costs.geodeRobotOre && obsidian >= costs.geodeRobotObsidian {
			ore -= costs.geodeRobotOre
			obsidian -= costs.geodeRobotObsidian
			deltaGeodeRobot = 1
		} else if ore >= costs.obsidianRobotOre && clay >= costs.obsidianRobotClay && rand.Float64() < 1.8/float64(1+obsidianRobot) {
			ore -= costs.obsidianRobotOre
			clay -= costs.obsidianRobotClay
			deltaObsidianRobot = 1
		} else if ore >= costs.clayRobotOre && rand.Float64() < 0.9/float64(1+clayRobot) {
			ore -= costs.clayRobotOre
			deltaClayRobot = 1
		} else if ore >= costs.oreRobotOre && oreRobot < 4 && rand.Float64() < 0.5/float64(1+oreRobot) {
			ore -= costs.oreRobotOre
			deltaOreRobot = 1
		}
		ore += oreRobot
		clay += clayRobot
		obsidian += obsidianRobot
		geode += geodeRobot
		//
		oreRobot += deltaOreRobot
		clayRobot += deltaClayRobot
		obsidianRobot += deltaObsidianRobot
		geodeRobot += deltaGeodeRobot
	}
	return geode
}

type robotCosts struct {
	oreRobotOre, clayRobotOre, obsidianRobotOre, obsidianRobotClay, geodeRobotOre, geodeRobotObsidian int
}

func main() {
	dir := "/home/xdavidliu/Documents/jetbrains-projects/goland/hello"
	fin, _ := os.Open(dir + "/example.txt")
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))

	quality := 0
	for sc.Scan() {
		words := strings.Split(sc.Text(), " ")
		indices := []int{6, 12, 18, 21, 27, 30}
		c := make([]int, len(indices))
		for i, v := range indices {
			c[i], _ = strconv.Atoi(words[v])
		}
		costs := robotCosts{
			c[0], c[1], c[2], c[3], c[4], c[5],
		}
		best := 0
		for i := 0; i < 10000000; i++ {
			s := solveOnce(costs)
			if s > best {
				best = s
			}
		}
		secondWord := words[1]
		id, _ := strconv.Atoi(secondWord[:len(secondWord)-1])
		quality += best * id
		fmt.Println(id, best)
	}
	fmt.Println("part 1 =", quality)
	// part 1 = 1603 too low
}

/*
1 2
2 3
3 5
4 7
5 0
6 0
7 2
8 9
9 0
10 3
11 3
12 3
13 2
14 6
15 3    <- 2 on another one; got 1588
16 0
17 0
18 14
19 0
20 1
21 0
22 13
23 0
24 0
25 12
26 0
27 1
28 0
29 3
30 8
part 1 = 1603
*/
