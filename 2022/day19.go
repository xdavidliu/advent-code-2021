package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type blueprint struct {
	oreCost, clayCost,
	obsidianCostOre, obsidianCostClay,
	geodeCostOre, geodeCostObsidian byte
}

type state struct {
	t, ore, clay, obsidian, geode,
	oreRob, clayRob, obsidianRob, geodeRob byte
}

func (s *state) collect() {
	s.ore += s.oreRob
	s.clay += s.clayRob
	s.obsidian += s.obsidianRob
	s.geode += s.geodeRob
	s.t++
}

type queue struct {
	data  []state
	front int
}

func (q *queue) add(s state) {
	q.data = append(q.data, s)
}

func (q *queue) remove() (s state, ok bool) {
	if q.front >= len(q.data) {
		ok = false
		return
	}
	s = q.data[q.front]
	q.front++
	if 2*q.front >= len(q.data) {
		count := len(q.data) - q.front
		for i := 0; i < count; i++ {
			q.data[i] = q.data[i+q.front]
		}
		q.data = q.data[:count]
		q.front = 0
	}
	ok = true
	return
}

func main() {
	dir := "/usr/local/google/home/xdavidliu/Desktop"
	fin, _ := os.Open(dir + "/data.txt")
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	var blues []blueprint
	for sc.Scan() {
		words := strings.Split(sc.Text(), " ")
		convWord := func(i int) byte {
			c, _ := strconv.Atoi(words[i])
			return byte(c)
		}
		b := blueprint{
			oreCost: convWord(6), clayCost: convWord(12),
			obsidianCostOre: convWord(18), obsidianCostClay: convWord(21),
			geodeCostOre: convWord(27), geodeCostObsidian: convWord(30),
		}
		blues = append(blues, b)
	}
	fmt.Println("part 1 =", part1(blues)) // 1613
	firstThree := [...]blueprint{blues[0], blues[1], blues[2]}
	fmt.Println("part 2 =", part2(firstThree)) // 46816
}

func part2(blues [3]blueprint) int {
	prod := 1
	for _, b := range blues {
		prod *= solve(b, 32)
	}
	return prod
}

func part1(blues []blueprint) int {
	ch := make(chan int)
	results := make([]int, len(blues))
	routine := func(i int) {
		r := solve(blues[i], byte(24))
		ch <- (i + 1) * r
	}
	for i := range blues {
		go routine(i)
	}
	for i := range results {
		results[i] = <-ch
	}
	total := 0
	for _, v := range results {
		total += v
	}
	return total
}

func max(bs []byte) (m byte) {
	for _, b := range bs {
		if b > m {
			m = b
		}
	}
	return
}

func solve(b blueprint, end byte) int {
	seen := make(map[state]bool)
	var q queue
	start := state{oreRob: 1}
	q.add(start)
	seen[start] = true
	s, ok := q.remove()
	var best byte
	possiblyAdd := func(t state) {
		if !seen[t] {
			q.add(t)
			seen[t] = true
		}
	}
	highCostOre := max([]byte{b.oreCost, b.clayCost, b.obsidianCostOre, b.obsidianCostOre})
	for ; ok; s, ok = q.remove() {
		if s.t == end {
			if s.geode > best {
				best = s.geode
			}
			continue
		}
		if s.obsidian >= b.geodeCostObsidian && s.ore >= b.geodeCostOre {
			t := s
			t.obsidian -= b.geodeCostObsidian
			t.ore -= b.geodeCostOre
			t.collect() // must do BEFORE incrementing robots
			t.geodeRob++
			possiblyAdd(t)
			continue // technically hack but should be obvious
		}
		if s.clay >= b.obsidianCostClay && s.ore >= b.obsidianCostOre && s.obsidianRob < b.geodeCostObsidian {
			t := s
			t.clay -= b.obsidianCostClay
			t.ore -= b.obsidianCostOre
			t.collect()
			t.obsidianRob++
			possiblyAdd(t)
		}
		if s.ore >= b.clayCost && s.clayRob < b.obsidianCostClay {
			t := s
			t.ore -= b.clayCost
			t.collect()
			t.clayRob++
			possiblyAdd(t)
		}
		if s.ore >= b.oreCost && s.oreRob < highCostOre {
			t := s
			t.ore -= b.oreCost
			t.collect()
			t.oreRob++
			possiblyAdd(t)
		}
		t := s // for the case of not buying any robots
		t.collect()
		possiblyAdd(t)
	}
	return int(best)
}
