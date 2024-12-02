import java.io.File

val initialPrograms = 'a'..'p'
val buffer = CharArray(initialPrograms.count())

fun main() {
    val input = File("/tmp/data.txt").bufferedReader().readLine()
    val moves = input.split(',')
    val initialStr = initialPrograms.joinToString("")
    val seen = mutableMapOf(initialStr to 0)
    val seenList = mutableListOf(initialStr)
    val programs = initialPrograms.joinToString("").toCharArray()
    var firstIndex = -1
    var secondIndex = -1
    var repeat = ""
    for (iter in 1..1000) {  // reasonable number, adjust if necessary
        for (move in moves) {
            doMove(move, programs)
        }
        val str = programs.joinToString("")
        seen[str]?.let {
            firstIndex = it
            secondIndex = iter
            repeat = str
        }
        if (repeat.isNotEmpty()) {
            break
        }
        seen[str] = iter
        seenList.add(str)
        if (iter == 1) {
            println("part 1 = $str")  // fgmobeaijhdpkcln
        }
    }
    if (firstIndex == -1) {
        println("failed to find repeat; increase iter")
        return
    }
    val ind = (1_000_000_000 - firstIndex) % (secondIndex - firstIndex)
    println("part 2 = ${seenList[ind]}")  // lgmkacfjbopednhi
}

fun reverse(arr: CharArray, first: Int, last: Int) {
    var first = first
    var last = last
    while (first < last) {
        val temp = arr[first]
        arr[first] = arr[last]
        arr[last] = temp
        ++first
        --last
    }
}

fun spin(arr: CharArray, x: Int) {
    if (x == 0) { return }
    val firstEnd = arr.size - x - 1
    reverse(arr, 0, firstEnd)
    reverse(arr, firstEnd + 1, arr.size - 1)
    reverse(arr, 0, arr.size - 1)
}

// indeed 0-indexed: from description:
// "x3/4, swapping the last two programs: eabdc."
fun exchange(arr: CharArray, a: Int, b: Int) {
    if (a != b) {
        val temp = arr[a]
        arr[a] = arr[b]
        arr[b] = temp
    }
}

val spinReg = Regex("s(\\d+)")
val exchangeReg = Regex("x(\\d+)/(\\d+)")
val partnerReg = Regex("p(\\w)/(\\w)")

fun doMove(move: String, programs: CharArray) {
    // no obvious way to do this with a when
    // https://discuss.kotlinlang.org/t/using-regex-in-a-when/1794/2
    spinReg.matchEntire(move)?.groupValues?.let {
        spin(programs, it[1].toInt())
        return
    }
    // above return works: a print statement here wouldn't print
    exchangeReg.matchEntire(move)?.groupValues?.let {
        exchange(programs, it[1].toInt(), it[2].toInt())
        return
    }
    partnerReg.matchEntire(move)?.groupValues?.let {
        exchange(programs, programs.indexOf(it[1][0]), programs.indexOf(it[2][0]))
        return
    }
}
