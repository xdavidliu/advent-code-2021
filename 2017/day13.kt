import java.io.File

class Layer(val depth: Int, val range: Int)

fun splitLine(line: String): Layer {
    val words = line.split(": ")
    return Layer(depth = words[0].toInt(), range = words[1].toInt())
}

fun run(delay: Int, layers: List<Layer>, stopEarly: Boolean = false): Int {
    var score = 0
    for (layer in layers) {
        if (0 == (layer.depth + delay) % (2 * layer.range - 2)) {
            if (stopEarly) {
                return -1
            }
            score += layer.depth * layer.range
        }
    }
    return score
}

fun main() {
    val layers = File("/home/xdavidliu/Documents/data.txt")
        .bufferedReader().lines().map { splitLine(it) }.toList()
    // at beginning of picosecond k:
    // - packet is at depth k, then moves to k + 1
    // - scanner is at position k, then moves to (k + 1) % range
    // - want to count when scanner is at position 0 at beginning of picosecond
    val ans1 = run(0, layers)
    println("part 1 = $ans1")  // 2160
    val ans2 = generateSequence(0) {it + 1}.find { 0 == run(it, layers, true) }
    println("part 2 = $ans2")  // 3907470
}
