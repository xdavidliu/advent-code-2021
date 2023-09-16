fun main() {
    solve(256, listOf(46,41,212,83,1,255,157,65,139,52,39,254,2,86,0,204))
}

fun solve(size: Int, lengths: List<Int>) {
    val list = (0..< size).toMutableList()
    var current = 0
    var skip = 0
    for (length in lengths) {
        circularReverse(list, current, length)
        current = (current + length + skip) % list.size
        ++skip
    }
    val ans = list[0] * list[1]
    println("part 1 = $ans")
}

fun circularReverse(list: MutableList<Int>, current: Int, length: Int) {
    var first = current
    var last = (current + length - 1 + list.size) % list.size
    repeat(length / 2) {
        list.swap(first, last)
        first = (first + 1) % list.size
        last = (last - 1 + list.size) % list.size
    }
}

fun MutableList<Int>.swap(i: Int, k: Int) {
    val temp = this[i]
    this[i] = this[k]
    this[k] = temp
}
