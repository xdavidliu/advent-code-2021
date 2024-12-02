import java.io.File

fun main() {
    val txt = File("/home/xdavidliu/Documents/data.txt")
        .bufferedReader().useLines { solve(it.first()) }
}

fun solve(line: String) {
    var depth = 0
    var score = 0
    var count = 0
    var bang = false
    var garbage = false
    for (c in line) {
        if (bang) {
            bang = false
            continue
        }
        if (garbage) {
            when (c) {
                '!' -> bang = true
                '>' -> garbage = false
                else -> ++count
            }
        } else {
            when (c) {
                '<' -> garbage = true
                '{' -> ++depth
                '}' -> {
                    score += depth  // ???
                    --depth
                }
            }
        }
    }
    println("part 1 = $score")
    println("part 2 = $count")
}
