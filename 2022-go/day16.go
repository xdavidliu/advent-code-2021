package main

import (
	"bufio"
	"cmp"
	"fmt"
	"math"
	"os"
	"regexp"
	"slices"
	"strconv"
	"strings"
)

func main() {
	filename := "/home/xdavidliu/Documents/aoc/input16.txt"
	pr := readProblem(filename)
	solve(&pr)
}

func solve(pr *problem) {
	computeDistances(pr)
	computeNonzeroAdjacent(pr)
	// make nonzero adjacent part of pr
	best := math.MinInt
	recurse("AA", []string{}, 0, 30, &best, pr)
	fmt.Println("part 1 =", best) // 1857
	pr.record = make(map[string][]recordElem)
	recurse("AA", []string{}, 0, 26, &best, pr)
	var recordVals [][]recordElem
	for _, elems := range pr.record {
		// reverse so high scores first
		slices.SortFunc(elems, func(a recordElem, b recordElem) int {
			return cmp.Compare(b.score, a.score)
		})
		recordVals = append(recordVals, elems)
	}
	otherBest := math.MinInt
	for i1 := 0; i1+1 < len(recordVals); i1++ {
		for i2 := i1 + 1; i2 < len(recordVals); i2++ {
			for _, x1 := range recordVals[i1] {
				for _, x2 := range recordVals[i2] {
					if x1.score+x2.score <= otherBest {
						break // because sorted, so next score2 is only worse
					}
					overlap := false
					for _, y1 := range x1.seen {
						if slices.Contains(x2.seen, y1) {
							overlap = true
							break
						}
					}
					if !overlap {
						otherBest = x1.score + x2.score
						break
					}
				}
			}
		}
	}
	fmt.Println("part 2 =", otherBest) // 2536
}

type problem struct {
	nonzeroValves   []string
	rateOf          map[string]int
	adjacent        map[string][]string
	distances       map[string]int
	nonzeroAdjacent map[string][]string
	record          map[string][]recordElem
}

type recordElem struct {
	score int
	seen  []string
}

func readProblem(filename string) problem {
	pr := problem{}
	pr.rateOf = make(map[string]int)
	pr.adjacent = make(map[string][]string)
	pr.distances = make(map[string]int)
	pr.nonzeroAdjacent = make(map[string][]string)
	pr.record = make(map[string][]recordElem)
	fin, _ := os.Open(filename)
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	re := regexp.MustCompile(`Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? ([ \w,]+)`)
	for sc.Scan() {
		mat := re.FindStringSubmatch(sc.Text())
		valve := mat[1]
		rate, _ := strconv.Atoi(mat[2])
		if rate != 0 {
			pr.nonzeroValves = append(pr.nonzeroValves, valve)
			pr.rateOf[valve] = rate
		}
		pr.adjacent[valve] = strings.Split(mat[3], ", ")
	}
	return pr
}

type elem struct {
	valve string
	dist  int
}

func shortestDist(origin string, destination string, pr *problem) int {
	if origin == destination {
		return 0
	}
	ans, ok := pr.distances[origin+" "+destination]
	if ok {
		return ans
	}
	q := queue[elem]{}
	q.add(elem{origin, 0})
	seen := make(set[string])
	seen.insert(origin)
	for !q.isEmpty() {
		x := q.remove()
		for _, nb := range pr.adjacent[x.valve] {
			if nb == destination {
				pr.distances[origin+" "+destination] = 1 + x.dist
				return 1 + x.dist
			}
			if !seen.contains(nb) {
				seen.insert(nb)
				q.add(elem{nb, 1 + x.dist})
			}
		}
	}
	return math.MaxInt
}

func computeDistances(pr *problem) {
	start := "AA"
	for _, origin := range pr.nonzeroValves {
		shortestDist(start, origin, pr)
		for _, destination := range pr.nonzeroValves {
			shortestDist(origin, destination, pr)
		}
	}
}

func computeNonzeroAdjacent(pr *problem) {
	// todo: rename this variable, since it includes start
	withStart := appendToCopy(pr.nonzeroValves, "AA")
	for _, valve := range withStart {
		var others []string
		for _, nb := range pr.nonzeroValves {
			if nb != valve {
				others = append(others, nb)
			}
		}
		slices.SortFunc(others, func(a string, b string) int {
			x, _ := pr.distances[valve+" "+a]
			y, _ := pr.distances[valve+" "+b]
			return cmp.Compare(x, y)
		})
		pr.nonzeroAdjacent[valve] = others
	}
}

const param = 5 // play around with this to trade off running time with score

func recurse(
	valve string, seen []string, score int, timeLeft int,
	best *int, pr *problem) {
	others := pr.nonzeroAdjacent[valve]
	if score > 0 {
		pr.record[seen[0]] = append(pr.record[seen[0]], recordElem{score, seen})
	}
	*best = max(*best, score)
	for _, other := range others[:param] {
		if slices.Contains(seen, other) {
			continue
		}
		dist := pr.distances[valve+" "+other]
		if dist+1 < timeLeft {
			newTimeLeft := timeLeft - dist - 1
			newScore := score + newTimeLeft*pr.rateOf[other]
			newSeen := appendToCopy(seen, other)
			recurse(other, newSeen, newScore, newTimeLeft, best, pr)
		}
	}
}
