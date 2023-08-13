package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	solve()
	//
	// after this, put all the triplets from csvToInts into
	// a map, then iterate through the keys of the map and
	// for all 6 faces see if the other cube is in the map.
	// count all those and divide by 2 cuz u double counted, then
	// take len(map) * 6 which is total
	// # faces, subtract count shared from above
}

func csvToVec(line string) vec {
	words := strings.Split(line, ",")
	var ns vec
	for i := 0; i < d; i++ {
		ns[i], _ = strconv.Atoi(words[i])
	}
	return ns
}

const d = 3

// do not change this to slice, since countShared depends on this
// being an array, not slice
type vec [d]int

var signs = [...]int{1, -1}

func countShared(v vec, cubes map[vec]bool) int {
	count := 0

	for i := 0; i < d; i++ {
		w := v // copy because v is array not slice
		for _, sign := range signs {
			w[i] = v[i] + sign
			if cubes[w] {
				count++
			}
		}
	}
	return count
}

func part1(cubes map[vec]bool) {
	count := 0
	for k, _ := range cubes {
		count += countShared(k, cubes)
	}
	// may be tempted to divide by 2 because double counted,
	// i.e. count includes both
	// A shares face X with B
	// B shares face X with A
	// However, you do NOT need to divide by 2, since when you
	// multiply by 6, you're counting the side twice there TOO,
	// hence the count UN-divided by 2 exactly cancels out the
	// double counting when you * 6.
	part1 := len(cubes)*6 - count // NOT count / 2
	fmt.Println("part 1 =", part1)
}

type queue struct {
	// default-constructed slice is same as make([]vec, 0),
	// so appending onto it will work and NOT panic
	data  []vec
	front int
}

func (q *queue) add(v vec) {
	q.data = append(q.data, v)
}

// latest elem in queue is ALWAYS at len(data)-1
// earliest elem in queue is ALWAYS at q.front
func (q *queue) remove() vec {
	if q.front >= len(q.data) {
		panic(nil)
	}
	v := q.data[q.front]
	q.front++
	if 2*q.front >= len(q.data) {
		newLen := len(q.data) - q.front
		for i := 0; i < newLen; i++ {
			q.data[i] = q.data[i+q.front]
		}
		q.data = q.data[:newLen]
		q.front = 0
	}
	return v
}

// get an air cell outside by returning highest + 1
// in all dimensions
func outside(cubes map[vec]bool) (out vec) {
	// assume all data is positive; if I wanted to be really
	// rigorous I would set all components of v to MinInt here
	for c, _ := range cubes {
		for i := 0; i < d; i++ {
			if c[i] > out[i] {
				out[i] = c[i]
			}
		}
	}
	for i := 0; i < d; i++ {
		out[i]++
	}
	return
}

func part2(cubes map[vec]bool) {
	faceCount := 0
	// default actually works but we'll be explicit here
	q := queue{
		data:  make([]vec, 0),
		front: 0,
	}
	q.add(outside(cubes))
	airSeen := make(map[vec]bool)
	// q will never be empty because you are exploring outwards
	// in a spherical wave. Use arbitrary value here; tune it to
	// get answer for part 2
	// for example, 3000 is enough to get the answer 58
	// for data, 200k is enough to get 2008
	for len(airSeen) < 200000 {
		v := q.remove()
		for i := 0; i < d; i++ {
			for _, s := range signs {
				w := v // array copy
				w[i] += s
				if cubes[w] {
					faceCount++
				} else if !airSeen[w] {
					airSeen[w] = true
					q.add(w)
				}
			}
		}
	}
	// make starting point 1 past highest value of all three d
	// have queue of vecs, enqueue the starting point
	// have map of seen air cells
	// Actually you don't NEED map of seen faces, since a face
	// cannot be seen more than once: air cell can't be visited more than
	// once, and other side of face is a cube, which cannot be visited ever!
	// hence don't even need to represent face
	// each time you dequeue an element (it is an air cell), look
	// at each of 6 directions. If it's an air cell, check if it was seen,
	// and if not, enqueue it and also put it in seen. If it was seen, skip
	// it. If it's not an air cell, then it's a face, and add to number
	// of faces seen.
	fmt.Println("part 2 =", faceCount)
}

func solve() {
	dir := "/home/xdavidliu/Documents/jetbrains-projects/goland/hello"
	fin, _ := os.Open(dir + "/data.txt")
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	cubes := make(map[vec]bool, 2300) // approx length of data.txt
	for sc.Scan() {
		cubes[csvToVec(sc.Text())] = true
	}
	part1(cubes)
	part2(cubes)
}
