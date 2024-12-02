import java.io.File

fun main() {
    part1()
    part2()
}

const val nws = "(\\S+)"  // non whitespace, captured
val setPat = Regex("set $nws $nws")
val subPat = Regex("sub $nws $nws")
val mulPat = Regex("mul $nws $nws")
val jnzPat = Regex("jnz $nws $nws")

val lines = File("/tmp/data.txt").readLines()
val register = mutableMapOf<String, Long>()
var position = 0
var mulCount = 0

fun value(word: String) = word.toLongOrNull() ?: register.getOrDefault(word, 0)

fun perform() {
    setPat.matchEntire(lines[position])?.groupValues?.let {
        register[it[1]] = value(it[2])
        ++position
        return
    }
    subPat.matchEntire(lines[position])?.groupValues?.let {
        val key = it[1]
        register[key] = register.getOrDefault(key, 0L) - value(it[2])
        ++position
        return
    }
    mulPat.matchEntire(lines[position])?.groupValues?.let {
        val key = it[1]
        register[key] = register.getOrDefault(key, 0L) * value(it[2])
        ++position
        ++mulCount
        return
    }
    jnzPat.matchEntire(lines[position])?.groupValues?.let {
        if (value(it[1]) != 0L) {
            position += value(it[2]).toInt()
        } else {
            ++position
        }
        return
    }
}

fun part1() {
    while (position in lines.indices) {
        perform()
    }
    println("part 1 = $mulCount")  // 4225
}

/*
part 2:

(indented to indicate inner "blocks")

set b 67
set c b          a = 1,   b = c = 67
jnz a 2
jnz 1 5
mul b 100
sub b -100000
set c b
sub c -17000      a = 1,   b = 106700,   c = 123700
    set f 1        b = Z     starts 106700, increases 17 every loop, never stops
    set d 2
        set e 2         d = W   starts 2, increases by 1 every loop, until W = Z - 1
            set g d       e = X   starts 2, increases by 1 every loop, until X = Z - 1
            mul g e
            sub g b       g = W X - Z
            jnz g 2
            set f 0       set iff Z is composite
            sub e -1
            set g e
            sub g b       g = X + 1 - Z
            jnz g -8
        sub d -1       d = W + 1
        set g d
        sub g b        g = W + 1 - Z
        jnz g -13
    jnz f 2
    sub h -1     happens each time f = 0, i.e. Z is composite
    set g b
    sub g c      g = Z - 123700    exactly 1000 iters before g = 0 here
    jnz g 2
    jnz 1 3     terminates
    sub b -17
    jnz 1 -23

 so count number of composite numbers for Z from 106700 to 123700 step 17
 inclusive in both ends
 */

fun isComposite(k: Int): Boolean {
    var p = 2
    while (p * p <= k) {
        if (k % p == 0) { return true }
        ++p
    }
    return false
}

fun part2() {
    val ans = (106700..123700 step 17).count { isComposite(it) }
    println("part 2 = $ans")  // 905
}
