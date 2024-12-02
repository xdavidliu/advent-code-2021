import java.io.File

fun condition(a: Int, op: String, b: Int) =
    when(op) {
        "==" -> a == b
        "!=" -> a != b
        "<=" -> a <= b
        ">=" -> a >= b
        "<" -> a < b
        ">" -> a > b
        else -> error("invalid op")
    }

//                            1      2         3         4     5      6
val regex = Regex("""(\w+) (inc|dec) (-?\d+) if (\w+) (\S+) (-?\d+)""")

var highest = Int.MIN_VALUE

fun execute(line: String, regVals: MutableMap<String, Int>) {
    val m = regex.matchEntire(line)!!.groupValues
    val lhs = regVals.getOrDefault(m[4], 0)
    if (condition(lhs, m[5], m[6].toInt())) {
        val dest = m[1]
        val sgn = if (m[2] == "inc") { 1 } else { -1 }
        val newVal = regVals.getOrDefault(dest, 0) + sgn * m[3].toInt()
        if (newVal > highest) { highest = newVal }
        regVals[dest] = newVal
    }
}

fun main() {
    val regVals = mutableMapOf<String, Int>()
    File("/home/xdavidliu/Documents/aoc/input08.txt")
        .bufferedReader().readLines().forEach { execute(it, regVals) }
    val p1 = regVals.values.max()
    println("part 1 = $p1")  // 4888
    println("part 2 = $highest")  // 7774
}
