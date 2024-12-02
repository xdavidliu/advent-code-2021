val a0: Long = 699
val b0: Long = 124
val n: Long = 2147483647

fun main() {
    var a = a0
    var b = b0
    val t: Long = 65536  // 2^16
    var count = 0
    fun runA() { a = (a * 16807) % n }
    fun runB() { b = (b * 48271) % n }
    for (i in 1..40_000_000) {
        runA()
        runB()
        if (a % t == b % t) { ++count }
    }
    println("part 1 = $count")  // 600
    a = a0
    b = b0
    count = 0
    for (i in 1..5_000_000) {
        do {
            runA()
        } while (a % 4L != 0L)
        do {
            runB()
        } while (b % 8L != 0L)
        if (a % t == b % t) { ++count }
    }
    println("part 2 = $count")  // 313
}
