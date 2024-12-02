import java.io.File

fun isValid(phrase: String, sort: Boolean): Boolean {
    val words = phrase.split(" ")
    val seen = mutableSetOf<String>()
    for (w in words) {
        val x = if (sort) {
            w.toCharArray().sorted().joinToString("")
        } else { w }
        if (!seen.add(x)) {
            return false
        }
    }
    return true
}

fun main() {
    val lines = File("/home/xdavidliu/Documents/aoc/input04.txt")
        .bufferedReader()
        .readLines()
    val p1 = lines.count { isValid(it, sort = false) }
    println("part 1 = $p1")  // 451
    val p2 = lines.count { isValid(it, sort = true) }
    println("part 2 = $p2")  // 223
}
