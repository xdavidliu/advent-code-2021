package main

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

type queue struct {
	front []int
	back  []int
}

func pop[T](a *[]int) T {
	end := len(*a) - 1
	x := (*a)[end]
	*a = (*a)[:end]
	return x
}

func push(x int, a *[]int) {
	(*a) = append(*a, x)
}

func makeQueue() queue {
	return queue{nil, nil}
}

func (q *queue) add(x int) {
	q.back = append(q.back, x)
}

func (q *queue) remove() int {
	if 0 == len(q.front) {
		for 1 < len(q.back) {
			end := len(q.back) - 1
			x := q.back[end]
			q.back = q.back[:end]
			q.front = append(q.front, x)
		}
	}
}

