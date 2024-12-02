import java.io.File

fun <T> dropAt(lst: List<T>, ind: Int) = lst.take(ind) + lst.drop(ind + 1)

fun strength(port: List<Int>) = port[0] + port[1]

var best = Int.MIN_VALUE
var shortest = Int.MAX_VALUE
var bestWithShortest = Int.MIN_VALUE

fun recurse(port: List<Int>, ports: List<List<Int>>, want: Int, total: Int) {
    var found = false
    for ((i, other) in ports.withIndex()) {
        if (want in other) {
            val nextWant = other[1 - other.indexOf(want)]
            recurse(other, dropAt(ports, i), nextWant, total + strength(other))
            found = true
        }
    }
    if (!found && total > best) {
        best = total
    }
    // longest bridge means shortest "remaining" ports
    if (shortest > ports.size || shortest == ports.size && total > bestWithShortest) {
        shortest = ports.size
        bestWithShortest = total
    }
}

fun main() {
    val ports = File("/tmp/data.txt").readLines().map { line ->
        line.split("/").map { word -> word.toInt() }
    }
    for ((i, port) in ports.withIndex()) {
        if (0 in port) {
            val want = port[1 - port.indexOf(0)]
            recurse(port, dropAt(ports, i), want, strength(port))
        }
    }
    println("part 1 = $best")  // 1695
    println("part 2 = $bestWithShortest")  // 1673
}
