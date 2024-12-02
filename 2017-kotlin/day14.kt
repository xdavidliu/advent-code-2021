// requires KnotHash.kt

fun main() {
    var total = 0
    val grid = mutableListOf<String>()
    for (k in 0..127) {
        val h = knotHashBits("vbqugkhl-$k")
        total += h.count { it == '1' }
        grid.add(h)
    }
    println("part 1 = $total")
    part2(grid)
}

fun bfs(p: Pair<Int, Int>, seen: MutableSet<Pair<Int, Int>>, grid: List<String>) {
    val q = ArrayDeque<Pair<Int, Int>>()
    q.add(p)
    seen.add(p)
    fun tryVisit(z: Pair<Int, Int>) {
        if ('1' == grid[z.first][z.second] && z !in seen) {
            seen.add(z)
            q.add(z)
        }
    }
    val n = grid.size
    while (q.isNotEmpty()) {
        val (i, k) = q.removeFirst()
        if (i > 0) { tryVisit(Pair(i-1, k)) }
        if (i < n-1) { tryVisit(Pair(i+1, k)) }
        if (k > 0) { tryVisit(Pair(i, k-1)) }
        if (k < n-1) { tryVisit(Pair(i, k+1)) }
    }
}

fun part2(grid: List<String>) {
    val seen = mutableSetOf<Pair<Int, Int>>()
    var components = 0
    val n = 128
    for (i in 0..<n) {
        for (k in 0..<n) {
            val p = Pair(i, k)
            if ('1' == grid[i][k] && p !in seen) {
                bfs(p, seen, grid)
                ++components
            }
        }
    }
    println("part 2 = $components")
}
