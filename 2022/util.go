package main

import (
	"bufio"
	"cmp"
	"os"
)

func abs(x int) int {
	if x >= 0 {
		return x
	} else {
		return -x
	}
}

func sign(x int) int {
	if x < 0 {
		return -1
	} else if x > 0 {
		return 1
	} else {
		return 0
	}
}

func minimum[T cmp.Ordered](xs []T) T {
	if 0 == len(xs) {
		panic("minimum")
	}
	m := xs[0]
	for _, x := range xs {
		m = min(x, m)
	}
	return m
}

func maximum[T cmp.Ordered](xs []T) T {
	if 0 == len(xs) {
		panic("maximum")
	}
	m := xs[0]
	for _, x := range xs {
		m = max(x, m)
	}
	return m
}

func readGrid(filename string) *[][]byte {
	var grid [][]byte
	fin, _ := os.Open(filename)
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	for sc.Scan() {
		grid = append(grid, []byte(sc.Text()))
	}
	return &grid
}

func makeGrid[T any](nr int, nc int) *[][]T {
	grid := make([][]T, nr)
	for i := range grid {
		grid[i] = make([]T, nc)
	}
	return &grid
}

type set[T comparable] map[T]bool

func (s *set[T]) contains(k T) bool {
	_, ok := (*s)[k]
	return ok
}

func (s *set[T]) insert(k T) {
	(*s)[k] = true
}

func (s *set[T]) remove(k T) {
	delete(*s, k)
}

type queue[T any] struct {
	front []T
	back  []T
}

func pop[T any](a *[]T) T {
	end := len(*a) - 1
	x := (*a)[end]
	*a = (*a)[:end]
	return x
}

func push[T any](x T, a *[]T) {
	*a = append(*a, x)
}

func (q *queue[T]) isEmpty() bool {
	return len(q.front) == 0 && len(q.back) == 0
}

func (q *queue[T]) copy() queue[T] {
	front := make([]T, len(q.front))
	back := make([]T, len(q.back))
	copy(front, q.front)
	copy(back, q.back)
	return queue[T]{front, back}
}

func (q *queue[T]) add(x T) {
	q.back = append(q.back, x)
}

func (q *queue[T]) remove() T {
	if 0 == len(q.front) {
		for 1 < len(q.back) {
			push(pop(&q.back), &q.front)
		}
		return pop(&q.back)
	} else {
		return pop(&q.front)
	}
}
