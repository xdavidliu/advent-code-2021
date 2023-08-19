package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"strconv"
	"strings"
	"time"
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
	dir := "/home/xdavidliu/Desktop"
	fin, _ := os.Open(dir + "/data-2022-day-19.txt")
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
	t0 := time.Now()
	fmt.Println("part 1 =", part1(blues)) // 1613
	t1 := time.Now()
	fmt.Println("part 1 took", time.Since(t0))
	firstThree := [...]blueprint{blues[0], blues[1], blues[2]}
	fmt.Println("part 2 =", part2(firstThree)) // 46816
	fmt.Println("part 2 took", time.Since(t1))
}

func part2(blues [3]blueprint) int {
	ch := make(chan int)
	routine := func(b blueprint) {
		ch <- solve(b, byte(32))
	}
	for _, b := range blues {
		go routine(b)
	}
	prod := 1
	for i := 0; i < 3; i++ {
		x := <-ch
		prod *= x
	}
	return prod
}

func part1(blues []blueprint) int {
	ch := make(chan int)
	routine := func(i int) {
		r := solve(blues[i], byte(24))
		ch <- (i + 1) * r
	}
	for i := range blues {
		go routine(i)
	}
	total := 0
	for _ = range blues {
		r := <-ch
		total += r
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

const enough = math.MaxUint8

func solve(b blueprint, end byte) int {
	seen := make(map[state]bool)
	var q queue
	start := state{oreRob: 1}
	q.add(start)
	seen[start] = true
	s, ok := q.remove()
	var best byte
	highCostOre := max([]byte{b.oreCost, b.clayCost, b.obsidianCostOre, b.obsidianCostOre})
	dedup := func(s *state) {
		if s.oreRob == highCostOre && s.ore > highCostOre {
			s.ore = highCostOre
		}
		// careful here; byte is uint8 so negative numbers don't exist
		if int(s.ore)-int(end-2-s.t)*int(highCostOre-s.oreRob) >= int(highCostOre) {
			s.oreRob = 0
			s.ore = enough
		}
		if s.t == end-4 {
			s.clayRob = 0 // end-5 to end-4 is last meaningful clay collection
			if s.clay >= b.obsidianCostClay {
				s.clay = b.obsidianCostClay // end-4 is last time making an obsidian
				// robot can potentially make a difference
			} else {
				s.clay = 0
			}
		} else if s.t == end-3 {
			s.clay = 0 // won't make any more obsidian robots so clay useless
		}
	}
	possiblyAdd := func(t state) {
		dedup(&t) // optional but makes it faster
		tzero := t
		tzero.t = 0
		// hack: because time is monotonically increasing in the BFS, previously
		// seen states regardless of time are strictly better
		if !seen[tzero] {
			q.add(t)
			seen[tzero] = true
		}
	}
	for ; ok; s, ok = q.remove() {
		if s.t == end-3 {
			// huge optimization!
			score := s.geode + 3*s.geodeRob
			// can I afford a geode robot right now?
			if s.ore >= b.geodeCostOre && s.obsidian >= b.geodeCostObsidian {
				score += 2
				if s.ore+s.oreRob >= 2*b.geodeCostOre && s.obsidian+s.obsidianRob >= 2*b.geodeCostObsidian {
					// can afford another one at end-2
					score++
				}
			} else if s.ore+s.oreRob >= b.geodeCostOre && s.obsidian+s.obsidianRob >= b.geodeCostObsidian {
				// cannot afford at end-3 but can afford at end-2
				score++ // geode robot only ready at end-1
			}
			if score > best {
				best = score
			}
			continue
		}
		// at end-4 it's still meaningful to make obsidian robots and ore robots
		if s.obsidian >= b.geodeCostObsidian && s.ore >= b.geodeCostOre {
			t := s
			t.obsidian -= b.geodeCostObsidian
			if t.ore != math.MaxUint8 {
				t.ore -= b.geodeCostOre
			}
			t.collect() // must do BEFORE incrementing robots
			t.geodeRob++
			possiblyAdd(t)
			continue // technically hack but should be obvious
		}
		t := s // for the case of not buying any robots
		t.collect()
		possiblyAdd(t)
		if s.clay >= b.obsidianCostClay && s.ore >= b.obsidianCostOre && s.obsidianRob < b.geodeCostObsidian {
			t := s
			t.clay -= b.obsidianCostClay
			if t.ore != enough {
				t.ore -= b.obsidianCostOre
			}
			t.collect()
			t.obsidianRob++
			possiblyAdd(t)
		}
		// end-6 is last time it's meaningful to make a clay robot, because then you
		// get the clay robot at end-5, get 1 clay, get the obsidian robot at end-3,
		// get one obsidian at end-2, then make a geode robot then.
		if s.t <= end-6 && s.ore >= b.clayCost && s.clayRob < b.obsidianCostClay {
			t := s
			if t.ore != enough {
				t.ore -= b.clayCost
			}
			t.collect()
			t.clayRob++
			possiblyAdd(t)
		}
		if t.ore != enough && s.ore >= b.oreCost && s.oreRob < highCostOre {
			t := s
			t.ore -= b.oreCost
			t.collect()
			t.oreRob++
			possiblyAdd(t)
		}
	}
	fmt.Println(len(seen)) // measure size of BFS
	// with all the above optimizations, each of the three in part 2 have like
	// 500k BFS size. Without the optimizations, each was like 8 million.
	return int(best)
}
