import java.io.File

// requires Long. If you use Int it gives wrong answer!

val instructions: List<String> =
    File("/tmp/data.txt").bufferedReader().lines().toList()

fun main() {
    part1()  // 7071
    part2()  // 8001
}

fun part1() {
    val program = Program()
    while (0 <= program.position && program.position < instructions.size) {
        program.perform()
    }
}

fun part2() {
    val program0 = TwoProgram()
    val program1 = TwoProgram()
    program0.other = program1
    program1.other = program0
    program1.set("p", 1)  // program ID
    var wasFailedReceive = false
    var current = program0  // either one is fine
    while (0 <= current.position && current.position < instructions.size) {
        val initialPosition = current.position
        current.perform()
        val positionStayed = current.position == initialPosition
        // two failed receives in a row
        if (wasFailedReceive && positionStayed) { break }
        wasFailedReceive = positionStayed
        current = if (current === program0) { program1 } else { program0 }
    }
    println("part 2 = ${program1.sendCount}")
}

class TwoProgram() : Program() {
    var other: TwoProgram? = null
    val que = ArrayDeque<Long>()
    var sendCount = 0
    override fun send(v: Long) {
        other?.que?.add(v)
        ++sendCount
        // ++position done in perform
    }
    override fun receive(s: String) {
        if (que.isNotEmpty()) {
            set(s, que.removeFirst())
            ++position  // note position no change if que empty
        }
    }
}

val nws = "(\\S+)"  // non whitespace
fun unaryIns(prefix: String) = Regex("$prefix $nws")
fun binaryIns(prefix: String) = Regex("$prefix $nws $nws")

val sndReg = unaryIns("snd")
val setReg = binaryIns("set")
val addReg = binaryIns("add")
val mulReg = binaryIns("mul")
val modReg = binaryIns("mod")
val rcvReg = unaryIns("rcv")
val jgzReg = binaryIns("jgz")

// cleanup todo: instructions can probably be a global variable
open class Program() {
    var played = -1L
    var position = 0
    val register = mutableMapOf<String, Long>()
    open fun send(v: Long) {
        played = v
    }
    fun value(word: String) =
        word.toLongOrNull() ?: register.getOrDefault(word, 0L)
    fun update(reg: String, f: (Long) -> Long) {
        register[reg] = f(register.getOrDefault(reg, 0L))
    }
    fun set(reg: String, v: Long) {
        update(reg) { _ -> v }
    }
    open fun receive(s: String) {
        if (0L != value(s)) {
            println("part 1 = $played")
            position = -1  // end for part 1
        } else {
            ++position
        }
    }
    fun perform() {
        val instruction = instructions[position]
        sndReg.matchEntire(instruction)?.groupValues?.let {
            send(value(it[1]))
            ++position
            return
        }
        setReg.matchEntire(instruction)?.groupValues?.let {
            // compute outside just to be safe
            set(it[1], value(it[2]))
            ++position
            return
        }
        addReg.matchEntire(instruction)?.groupValues?.let {
            val v = value(it[2])
            update(it[1]) { old -> old + v }
            ++position
            return
        }
        mulReg.matchEntire(instruction)?.groupValues?.let {
            val v = value(it[2])
            update(it[1]) { old -> old * v }
            ++position
            return
        }
        modReg.matchEntire(instruction)?.groupValues?.let {
            val v = value(it[2])
            // interestingly, first arg never negative, so doesn't matter which
            // definition of mod you use.
            // update(it[1]) { old -> Math.floorMod(old, v) }
            update(it[1]) { old -> old % v }
            ++position
            return
        }
        rcvReg.matchEntire(instruction)?.groupValues?.let {
            receive(it[1])
            return
        }
        jgzReg.matchEntire(instruction)?.groupValues?.let {
            if (0L < value(it[1])) {
                position += value(it[2]).toInt()
            } else {
                ++position
            }
            return
        }
        assert(false)  // every instruction in input should be valid
    }
}
