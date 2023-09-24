fun main() {
    var state = 'A'
    var slot = 0
    val ones = mutableSetOf<Int>()
    fun write1() { ones.add(slot) }
    fun write0() { ones.remove(slot) }
    fun left() { --slot }
    fun right() { ++slot }
    fun zero() = slot !in ones
    repeat(12173597) {
        when (state) {
            'A' -> {
                if (zero()) {
                    write1()
                    right()
                    state = 'B'
                } else {
                    write0()
                    left()
                    state = 'C'
                }
            }
            'B' -> {
                if (zero()) {
                    write1()
                    left()
                    state = 'A'
                } else {
                    write1()
                    right()
                    state = 'D'
                }
            }
            'C' -> {
                if (zero()) {
                    write1()
                    right()
                    state = 'A'
                } else {
                    write0()
                    left()
                    state = 'E'
                }
            }
            'D' -> {
                if (zero()) {
                    write1()
                    right()
                    state = 'A'
                } else {
                    write0()
                    right()
                    state = 'B'
                }
            }
            'E' -> {
                if (zero()) {
                    write1()
                    left()
                    state = 'F'
                } else {
                    write1()
                    left()
                    state = 'C'
                }
            }
            'F' -> {
                if (zero()) {
                    write1()
                    right()
                    state = 'D'
                } else {
                    write1()
                    right()
                    state = 'A'
                }
            }
        }
    }
    println("part 1 = ${ones.size}")  // 2870
    println("part 2 = just click the button")
}
