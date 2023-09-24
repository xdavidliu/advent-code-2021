import java.io.File

fun main() {
    solve()
}

fun solve() {
    val grid =
        File("/tmp/data.txt").bufferedReader().lines().toList()
    var row = 0
    var col = grid.first.indexOf('|')
    val height = grid.size
    val width = grid.first.length
    var dr = 1
    var dc = 0
    val seen = mutableListOf<Char>()
    var steps = 1  // starts with 1 step because assume came from top edge
    while (row in 0..<height && col in 0..<width) {
        val ch = grid[row][col]
        if (ch == '+') {
            if (dc == 0) {
                dr = 0
                // can't use '-' == because it could be a letter or a + there
                dc = if (col > 0 && ' ' != grid[row][col-1]) { -1 } else { 1 }
                // assume will never end on a +
            } else {  // dr == 0
                dc = 0
                dr = if (row > 0 && ' ' != grid[row-1][col]) { -1 } else { 1 }
            }
        } else if (ch.isLetter()) {
            seen.add(ch)
        } else if (ch == ' ') { break }
        row += dr
        col += dc
        ++steps
    }
    val part1 = seen.joinToString("")
    println("part 1 = $part1")  // SXWAIBUZY
    // subtract 1 because either went off edge or went past last letter to ' '
    println("part 2 = ${steps - 1}")  // 16676
}
