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
