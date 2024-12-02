fun knotHash(input: String) = toHex(denseHash(input))

fun knotHashBits(input: String) = toBinary(denseHash(input))

fun denseHash(input: String): List<Int> =
    reduce256(run(256, toSequence(input), 64))

fun toSequence(input: String) = input.map { it.code } + listOf(17, 31, 73, 47, 23)

// https://stackoverflow.com/a/56970434/2990344
fun toHex(small: List<Int>) = small.map { "%02x".format(it) }.joinToString("")

fun toBinary(small: List<Int>) = small.map { binaryFormat(it) }.joinToString("")

// cannot do "%08b" because doesn't work in C
// https://stackoverflow.com/a/35926790/2990344
fun binaryFormat(k: Int) = "%8s".format(Integer.toBinaryString(k)).replace(' ', '0')

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
