fun knotHash(input: String): String {
    val list = run(256, toSequence(input), 64)
    return toHex(reduce256(list))
}

fun toSequence(input: String) = input.map { it.code } + listOf(17, 31, 73, 47, 23)

fun toHex(small: List<Int>) = small.map { String.format("%02x", it) }.joinToString("")

fun reduce256(big: List<Int>): List<Int> {
    val small = mutableListOf<Int>()
    for (begin in 0 ..< 256 step 16) {
        var reducedValue = 0  // identity for XOR
        repeat(16) {
            reducedValue = reducedValue.xor(big[begin + it])
        }
        small.add(reducedValue)
    }
    return small
}

fun run(size: Int, lengths: List<Int>, rounds: Int = 1): List<Int> {
    val list = (0..< size).toMutableList()
    var current = 0
    var skip = 0
    repeat(rounds) {
        for (length in lengths) {
            circularReverse(list, current, length)
            current = (current + length + skip) % list.size
            ++skip
        }
    }
    return list
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
