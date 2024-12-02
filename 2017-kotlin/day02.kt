import java.io.File

fun parseNumbers(s: String): List<Int> {
    return s.split("\\s+".toRegex()).map { it.toInt() }
}

fun soleQuotient(xs: List<Int>): Int {
    for (x in xs) {
        for (y in xs) {
            if (x > y && x % y == 0) {
                return x / y
            }
        }
    }
    return 0
}

fun main() {
    val rd = File("/Users/xdavidliu/Documents/aoc/input02.txt").bufferedReader()
    val lists = rd.readLines().map { parseNumbers(it) }
    val p1 = lists.map { it.maxOrNull()!! - it.minOrNull()!! }.sum()
    println("part 1 = $p1")  // 44216
    val p2 = lists.map { soleQuotient(it) }.sum()
    println("part 2 = $p2")  // 320
}
