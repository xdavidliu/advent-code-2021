import java.io.File

fun solve(nums: IntArray, special: Boolean): Int {
    var i = 0
    var steps = 0
    while (i in nums.indices ) {
        val w = nums[i]
        nums[i] += if (!special || w < 3) { 1 } else { -1 }
        i += w
        ++steps
    }
    return steps
}

fun main() {
    val nums = File("/home/xdavidliu/Documents/aoc/input05.txt")
        .bufferedReader()
        .readLines()
        .map { it.toInt() }
        .toIntArray()
    val nums2 = nums.copyOf()
    val p1 = solve(nums, special = false)
    println("part 1 = $p1")  // 342669
    val p2 = solve(nums2, special = true)
    println("part 2 = $p2")  // 25136209
}
