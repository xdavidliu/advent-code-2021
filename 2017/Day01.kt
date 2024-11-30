import java.io.File

fun step_sum(t: String, d: Int): Int {
    var sum = 0
    for ((i, x) in t.withIndex()) {
        if (x == t[(i+d) % t.length]) {
            sum += x.digitToInt()
        }
    }
    return sum
}

fun main() {
    val t = File("/Users/xdavidliu/Documents/aoc/input01.txt")
        .readText().trimEnd()
    val p1 = step_sum(t, 1)
    println("part 1 = $p1")  // 1029
    val p2 = step_sum(t, t.length / 2)
    println("part 2 = $p2")  // 1220
}
