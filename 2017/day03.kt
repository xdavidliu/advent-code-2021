import kotlin.math.abs

fun main() {
    val input = 277678
    val p1 = computeDistance(input)
    println("part 1 = $p1")  // 475
    val p2 = firstLarger(input)
    println("part 2 = $p2")  // 279138
}

fun square(x: Int): Int {
    return x * x
}

fun findLayer(q: Int): Int {
    var k = 0
    while (q > square(2*k+1)) {
        ++k
    }
    return k
}

fun computeDistance(q: Int): Int {
    val k = findLayer(q)
    val lastCorner = square(2*k+1)
    var corner = square(2*k-1) + 2*k
    while (corner <= lastCorner) {
        if (q <= corner) {
            return k + abs(k - (corner - q))
        }
        corner += 2*k
    }
    error("distance unexpectedly not found")
}

fun findLayerMore(q: Int): Int {
    var z = 1
    var k = 0
    while (z < q) {
        z *= 25
        ++k
    }
    return k
}

fun createGrid(q: Int): Array<IntArray> {
    val k = findLayerMore(q)
    // actual bottom right corner grows faster than k^25 anyway; and for
    // q <= 25 we add one layer of zero padding on each side (as opposed to 2k+1)
    // for sentinels so we don't have to do boundary detection
    val n = 2*k+3
    return Array(n) { IntArray(n) {0} }
}

fun constSeq(v: Int, len: Int) = generateSequence(v) { it }.take(len)

fun sumNeighbors(grid: Array<IntArray>, r: Int, c: Int): Int {
    var sum = 0
    for (dr in listOf(-1, 0, 1)) {
        for (dc in listOf(-1, 0, 1)) {
            if (dr != 0 || dc != 0) {
                sum += grid[r+dr][c+dc]
            }
        }
    }
    return sum
}

fun firstLarger(q: Int): Int {
    val grid = createGrid(q)
    assert(grid.size % 2 == 1)
    var r = grid.size / 2
    var c = r
    var k = 0
    grid[r][c] = 1
    while (true) {
        ++k
        // first part is the one step for move to next layer
        val dr = constSeq(0, 1) + constSeq(-1, 2*k-1) + constSeq(0, 2*k) +
                constSeq(1, 2*k) + constSeq(0, 2*k)
        val dc = constSeq(1, 1) + constSeq(0, 2*k-1) + constSeq(-1, 2*k) +
                constSeq(0, 2*k) + constSeq(1, 2*k)
        dr.zip(dc).forEach {
            r += it.first
            c += it.second
            val x = sumNeighbors(grid, r, c)
            // println("r = ${r-grid.size/2}, c = ${c-grid.size/2}, x = $x")
            if (x > q) {
                return x
            }
            grid[r][c] = x
        }
    }
}

/*
Part 1:

layer 0: x = 0 y = 0
layer 1:  1^2+1 to 3^2

layer 2:  3^2+1 to 5^2
          4 steps from one corner to other

layer 3:  5^2+1 to 7^2
          6 steps from one corner to other

layer k:  (2k-1)^2+1 to (2k+1)^2
          2k steps from one corner to other


(2k-1)^2+4k           (2k-1)^2+2k



                       (2k-1)^2+1
(2k-1)^2+6k             (2k+1)^2

 */
