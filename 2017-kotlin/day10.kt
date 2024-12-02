// requires KnotHash.kt

fun main() {
    part1()  // 52070
    part2()  // 7f94112db4e32e19cf6502073c66f9bb
}

fun part2() {
    val input = "46,41,212,83,1,255,157,65,139,52,39,254,2,86,0,204"
    val ans = knotHash(input)
    println("part 2 = $ans")
}

fun part1() {
    val list = run(256, listOf(46,41,212,83,1,255,157,65,139,52,39,254,2,86,0,204))
    val ans = list[0] * list[1]
    println("part 1 = $ans")
}
