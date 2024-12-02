import java.io.File

fun main() {
    val m = readAdjacency("/home/xdavidliu/Documents/data.txt")
    val seen = mutableSetOf<Int>()
    val q = ArrayDeque<Int>()
    fun bfs(start: Int) {
        seen.add(start)
        q.add(start)
        while (q.isNotEmpty()) {
            val i = q.removeFirst()
            for (k in m[i]!!) {
                if (k !in seen) {
                    seen.add(k)
                    q.add(k)
                }
            }
        }
    }
    bfs(0)
    println("part 1 = ${seen.size}")  // 145
    var groups = 1
    for (start in m.keys) {
        if (start !in seen) {
            bfs(start)
            ++groups
        }
    }
    println("part 2 = $groups")  // 207
}

// there's probably a more elegant way to do this. See
// https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
// https://docs.oracle.com/en/java/javase/20/docs/api/java.base/java/util/regex/Pattern.html
fun readAdjacency(path: String): Map<Int, List<Int>> {
    // 0 <-> 412, 480, 777, 1453
    val reg = Regex("(\\d+) <-> (.+)")
    val m = mutableMapOf<Int, List<Int>>()
    File(path).bufferedReader().useLines {
        it.forEach {
            reg.matchEntire(it)?.groupValues?.let {
                val from = it[1].toInt()
                val to = it[2].splitToSequence(", ").map { it.toInt() }.toList()
                m[from] = to
            }
        }
    }
    return m
}
