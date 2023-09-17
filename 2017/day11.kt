import java.io.File
import kotlin.math.abs

fun main() {
    val input = File("/home/xdavidliu/Documents/data.txt")
        .useLines { it.first().split(',') }
    var x = 0
    var y = 0
    var high = 0
    for (dir in input) {
        when (dir) {
            "n" -> y += 2
            "s" -> y -= 2
            "nw" -> {
                ++y
                --x
            }
            "sw" -> {
                --y
                --x
            }
            "ne" -> {
                ++y
                ++x
            }
            "se" -> {
                --y
                ++x
            }
        }
        val d = distance(x, y)
        if (d > high) {
            high = d
        }
    }
    println("part 1 = ${distance(x, y)}")
    println("part 2 = $high")
    // part 1 = 877
    // part 2 = 1622
}

fun distance(x: Int, y: Int) = if (abs(y) <= abs(x)) abs(x) else (abs(y) + abs(x)) / 2
// To see why this works, draw a grid and look at points at distance 1, 2 and 3.
// Notice all points at e.g. 2 are shaped like a crystal from FF4. The crystal
// can be divided into four parts: two triangles with point at origin and "base"
// and left and right, and the other two parts are squares "standing on a corner"
// both above and below origin.
// If the point is in one of the triangles, distance is just |x|
// If point is in top square, move to highest point in the square (similar for
// bottom square. Can clearly see distance is (|y| + |x|) / 2.
// It is triangle if |y| <= |x|.
