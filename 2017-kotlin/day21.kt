import java.io.File

val rules = readRules("/tmp/data.txt")

fun countTrue(image: Array<BooleanArray>) = image.map { row -> row.count { it } }.sum()

fun main() {
    var image = arrayOf(
        booleanArrayOf(false, true, false),
        booleanArrayOf(false, false, true),
        booleanArrayOf(true, true, true)
    )
    repeat(5) {
        image = iterate(image)
    }
    println("part 1 = ${countTrue(image)}")  // 167
    repeat(18 - 5) {
        image = iterate(image)
    }
    println("part 2 = ${countTrue(image)}")  // 2425195
}

fun update(arr: Array<BooleanArray>, row: Int, col: Int, lst: List<List<Boolean>>) {
    for (dr in 0..<lst.size) {
        for (dc in 0..<lst.first.size) {
            arr[row+dr][col+dc] = lst[dr][dc]
        }
    }
}

fun iterate(image: Array<BooleanArray>): Array<BooleanArray> {
    val out: Array<BooleanArray>
    if (image.size % 2 == 0) {
        val newSize = image.size / 2 * 3
        out = Array(newSize) { BooleanArray(newSize) }
        for (row in 0..<image.size step 2) {
            for (col in 0..<image.size step 2) {
                val lhs = listOf(
                    listOf(image[row][col], image[row][col+1]),
                    listOf(image[row+1][col], image[row+1][col+1])
                )
                update(out, row / 2 * 3, col / 2 * 3, rules[lhs]!!)
            }
        }
    } else {  // divisible by 3
        val newSize = image.size / 3 * 4
        out = Array(newSize) { BooleanArray(newSize) }
        for (row in 0..<image.size step 3) {
            for (col in 0..<image.size step 3) {
                val lhs = listOf(
                    listOf(image[row][col], image[row][col+1], image[row][col+2]),
                    listOf(image[row+1][col], image[row+1][col+1], image[row+1][col+2]),
                    listOf(image[row+2][col], image[row+2][col+1], image[row+2][col+2])
                )
                update(out, row / 3 * 4, col / 3 * 4, rules[lhs]!!)
            }
        }
    }
    return out
}

fun readRules(path: String): Map<List<List<Boolean>>, List<List<Boolean>>> {
    val rules = mutableMapOf<List<List<Boolean>>, List<List<Boolean>>>()
    File(path).bufferedReader()
        .lines().map { parseRule(it) }.toList()
        .forEach {
            var lhs = it[0]
            var lhsFlipped = flipped(lhs)
            val rhs = it[1]
            rules[lhs] = rhs
            rules[lhsFlipped] = rhs
            repeat(3) {
                lhs = rotated(lhs)
                rules[lhs] = rhs
                lhsFlipped = rotated(lhsFlipped)
                rules[lhsFlipped] = rhs
            }
        }
    return rules
}

fun rotated(a: List<List<Boolean>>) =
    if (a.size == 2) {
        listOf(
            listOf(a[1][0], a[0][0]),
            listOf(a[1][1], a[0][1])
        )
    } else {  // 3
        listOf(
            listOf(a[2][0], a[1][0], a[0][0]),
            listOf(a[2][1], a[1][1], a[0][1]),
            listOf(a[2][2], a[1][2], a[0][2])
        )
    }

fun flipped(a: List<List<Boolean>>) =
    if (a.size == 2) {
        listOf(
            listOf(a[1][0], a[1][1]),
            listOf(a[0][0], a[0][1])
        )
    } else {
        listOf(
            listOf(a[2][0], a[2][1], a[2][2]),
            listOf(a[1][0], a[1][1], a[1][2]),
            listOf(a[0][0], a[0][1], a[0][2])
        )
    }

// ../.# => ##./#../...
fun parseRule(s: String) = s.split(" => ").map { parseGrid(it) }

// .#./..#/###
fun parseGrid(s: String) = s.split("/").map { parseRow(it) }

// .#.
fun parseRow(s: String) = s.map { it == '#' }
