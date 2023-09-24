import java.io.File

fun main() {
    part1()  // 5447
    part2()  // 2511705
}

fun readInfected(path: String): Set<Pair<Int, Int>> {
    val lines =
        File(path).bufferedReader().lineSequence().toList()
    val infected = mutableSetOf<Pair<Int, Int>>()
    for ((row, line) in lines.withIndex()) {
        for ((col, ch) in line.withIndex()) {
            if (ch == '#') { infected.add(row to col) }
        }
    }
    // begins in middle of map
    initialRow = lines.size / 2
    initialCol = lines.first.length / 2
    return infected
}

var initialRow: Int = -1  // will be initialized with readInfected
var initialCol: Int = -1
val originalInfected = readInfected("/tmp/data.txt")

fun part2() {
    val infected = originalInfected.toMutableSet()
    val flagged = mutableSetOf<Pair<Int, Int>>()
    val weakened = mutableSetOf<Pair<Int, Int>>()
    var row = initialRow
    var col = initialCol
    // still facing up
    var dr = -1
    var dc = 0
    var caused = 0
    repeat(10000000) {
        val temp = dr
        val point = row to col
        if (point in weakened) {
            weakened.remove(point)
            infected.add(point)
            ++caused
            // no turn
        } else if (point in infected) {
            infected.remove(point)
            flagged.add(point)
            dr = dc  // turn right
            dc = -temp
        } else if (point in flagged) {
            flagged.remove(point)
            // no set for clean
            dr = -dr  // reverse
            dc = -dc
        } else {  // clean
            weakened.add(point)
            dr = -dc   // turn left
            dc = temp
        }
        row += dr
        col += dc
    }
    println("part 2 = $caused")
}

fun part1() {
    val infected = originalInfected.toMutableSet()
    var row = initialRow
    var col = initialCol
    // facing up
    var dr = -1
    var dc = 0
    var caused = 0
    repeat(10_000) {
        val temp = dr
        if (row to col in infected) {  // turn right
            dr = dc
            dc = -temp
            infected.remove(row to col)
        } else {  // turn left
            dr = -dc
            dc = temp
            infected.add(row to col)
            ++caused
        }
        row += dr
        col += dc
    }
    println("part 1 = $caused")
}
