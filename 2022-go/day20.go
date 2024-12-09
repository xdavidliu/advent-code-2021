package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func assert(b bool) {
	if !b {
		panic(nil)
	}
}

type node struct {
	val  int
	orig int
	next *node
	prev *node
}

func (x *node) look(d int) *node {
	z := x
	for ; d < 0; d++ {
		z = z.prev
	}
	for ; d > 0; d-- {
		z = z.next
	}
	return z
}

func (x *node) move() {
	if x.val == 0 {
		return
	}
	p, n := x.prev, x.next
	if x.val == 1 {
		nn := n.next
		p.next, n.next, x.next = n, x, nn
		nn.prev, x.prev, n.prev = x, n, p
	} else if x.val == -1 {
		pp := p.prev
		n.prev, p.prev, x.prev = p, x, pp
		pp.next, x.next, p.next = x, p, n
	} else { // abs(x.val) >= 2
		y := x.look(x.val)
		p.next, n.prev = n, p
		if x.val >= 2 {
			yn := y.next
			x.next, x.prev = yn, y
			y.next, yn.prev = x, x
		} else { // x.val <= -2
			yp := y.prev
			x.prev, x.next = yp, y
			y.prev, yp.next = x, x
		}
	}
}

func readInts(filePath string) []int {
	fin, _ := os.Open(filePath)
	defer fin.Close()
	sc := bufio.NewScanner(bufio.NewReader(fin))
	var input []int
	for sc.Scan() {
		k, _ := strconv.Atoi(sc.Text())
		input = append(input, k)
	}
	assert(len(input) >= 4) // swap funcs assume this
	return input
}

func reduce(orig int, period int) int {
	v := orig % period // % is remainder in Go, not mod
	if 2*v < -period {
		v += period
	} else if 2*v > period {
		v -= period
	}
	return v
}

func link(nums []int) (zero *node, nodes []*node) {
	zero = nil
	nodes = make([]*node, len(nums))
	var prev *node = nil
	for i := 0; i < len(nums); i++ {
		v := reduce(nums[i], len(nums)-1)
		x := &node{val: v, orig: nums[i], prev: prev, next: nil}
		if nums[i] == 0 {
			assert(zero == nil) // not expecting more than one
			// must be actually zero, not just equals zero modulo n-1
			zero = x
		}
		if prev != nil { // only the i = 0 case has nil
			prev.next = x
		}
		nodes[i] = x
		prev = x
	}
	head := nodes[0]
	last := nodes[len(nodes)-1]
	last.next = head
	head.prev = last
	assert(zero != nil)
	return
}

func solve(input []int, times int) int {
	zero, nodes := link(input)
	for i := 0; i < times; i++ {
		for _, x := range nodes {
			x.move()
		}
	}
	// do not use reduce here; reducing is for jumps, which are in gaps,
	// not on nodes, so there are some off-by-one shenanigans
	thousand := 1000 % len(nodes)
	one := zero.look(thousand)
	two := one.look(thousand)
	three := two.look(thousand)
	return one.orig + two.orig + three.orig
}

func main() {
	input := readInts("/home/xdavidliu/Desktop/data.txt")
	fmt.Println("part 1 =", solve(input, 1))
	for i := range input {
		input[i] *= 811589153
	}
	fmt.Println("part 2 =", solve(input, 10))
}
