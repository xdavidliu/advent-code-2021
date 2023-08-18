package main

import "fmt"

type blueprint struct {
	id, oreCost, clayCost,
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
	foo()
}

func foo() {
	blue := blueprint{
		id: 1, oreCost: 4, clayCost: 2,
		obsidianCostOre: 3, obsidianCostClay: 14,
		geodeCostOre: 2, geodeCostObsidian: 7,
	}
	fmt.Println(solve(blue))
}

const end = 24 // should be 24

func solve(b blueprint) (best byte) {
	seen := make(map[state]bool)
	var q queue
	start := state{oreRob: 1}
	q.add(start)
	seen[start] = true
	s, ok := q.remove()
	best = 0
	possiblyAdd := func(t state) {
		if !seen[t] {
			q.add(t)
			seen[t] = true
		}
	}
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
			t.collect()
			t.geodeRob++
			possiblyAdd(t)
		}
		if s.clay >= b.obsidianCostClay && s.ore >= b.obsidianCostOre {
			t := s
			t.clay -= b.obsidianCostClay
			t.ore -= b.obsidianCostOre
			t.collect()
			t.obsidianRob++
			possiblyAdd(t)
		}
		if s.ore >= b.clayCost {
			t := s
			t.ore -= b.clayCost
			t.collect()
			t.clayRob++
			possiblyAdd(t)
		}
		if s.ore >= b.oreCost && s.ore < 4 {
			t := s
			t.ore -= b.oreCost
			t.collect()
			t.oreRob++
			possiblyAdd(t)
		}
		t := s // also don't buy any robots
		t.collect()
		possiblyAdd(t)
	}
	return
}

// Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore.
// Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
