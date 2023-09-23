fun main() {
    val input = 301
    part1(input)
    part2(input)
}

fun part2(input: Int) {
    var currentIndex = 0
    var afterZero = 1
    var length = 2
    currentIndex = 1
    for (i in 2..50_000_000) {
        currentIndex = (currentIndex + input) % length
        ++length
        if (currentIndex == 0) { afterZero = i }
        // carefully pretend like I'm actually inserting a value
        currentIndex = (currentIndex + 1) % length
    }
    println("part 2 = $afterZero")  // 33601318
}

fun part1(input: Int) {
    val x = Node(0)
    var current = x
    var length = 1
    for (i in 1..2017) {
        repeat(input % length) {
            current = current.next
        }
        current.insert(i)
        ++length
        current = current.next
    }
    println("part 1 = ${current.next.value}")  // 1642
}

class Node(val value: Int) {
    var next: Node = this
    fun insert(value: Int) {
        val other = Node(value)
        other.next = next
        next = other
    }
}
