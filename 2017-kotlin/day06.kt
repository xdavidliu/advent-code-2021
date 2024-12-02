
fun findFirstMax(arr: IntArray): Pair<Int, Int> {
    var m = Int.MIN_VALUE
    var i = -1
    for ((k, x) in arr.withIndex()) {
        if (x > m) {
            m = x
            i = k
        }
    }
    return Pair(i, m)
}

fun cycleOnce(arr: IntArray) {
    var (i, m) = findFirstMax(arr)
    arr[i] = 0
    fun next(k: Int) = (k + 1) % arr.size
    i = next(i)
    while (m > 0) {
        ++arr[i]
        --m
        i = next(i)
    }
}

fun solve(arr: IntArray): Pair<Int, Int> {
    val seen = mutableSetOf(arr.toList())
    val seenAfter = mutableMapOf(Pair(arr.toList(), 0))
    var ops = 0
    while (true) {
        cycleOnce(arr)
        ++ops
        seenAfter.put(arr.toList(), ops)?.let {
            return Pair(it, ops)
        }
    }
}

fun main() {
    val arr = intArrayOf(14,0,15,12,11,11,3,5,1,6,8,4,9,1,8,4)
    val (a, b) = solve(arr)
    println("part 1 = $b")  // 11137
    println("part 2 = ${b - a}")  // 1037
}
