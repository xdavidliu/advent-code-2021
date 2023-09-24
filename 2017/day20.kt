import java.io.File

fun main() {
    val particles = File("/tmp/data.txt")
        .bufferedReader().lines().map { parse(it) }.toList()
    part1(particles)
    part2(particles.toMutableList())
}

fun part2(particles: MutableList<Particle>) {
    repeat (10_000) {  // not rigorous, but works
        for (particle in particles) {
            particle.update()
        }
        val groups = particles.groupBy { it.p }
        for (group in groups.values) {
            if (group.size > 1) {
                for (particle in group) {
                    particles.remove(particle)
                }
            }
        }
    }
    println("part 2 = ${particles.size}")  // 648
}

fun part1(particles: List<Particle>) {
    val low = particles.minOf { it.a.absSquare }
    val particlesWithLow = particles.filter { it.a.absSquare == low }
    val indices = particlesWithLow.map { particles.indexOf(it) }
    println("part 1 = one of these: ${indices}")  // [158, 243]  actually 243
}

// not vector because that's a data structure
data class Arrow(var x: Long, var y: Long, var z: Long) {
    // original value only
    val absSquare = x * x + y * y + z * z
    operator fun plusAssign(other: Arrow) {
        x += other.x
        y += other.y
        z += other.z
    }
}

data class Particle(val p: Arrow, val v: Arrow, val a: Arrow) {
    fun update() {
        v += a
        p += v
    }
}

const val num = "(-?\\d+)"
// p=<1199,-2918,1457>, v=<-13,115,-8>, a=<-7,8,-10>
val particleRegex =
    Regex("p=<$num,$num,$num>, v=<$num,$num,$num>, a=<$num,$num,$num>")

fun parse(s: String): Particle {
    particleRegex.matchEntire(s)!!
        .groupValues.drop(1).map { it.toLong() }.let {
            return Particle (
                Arrow(it[0], it[1], it[2]),
                Arrow(it[3], it[4], it[5]),
                Arrow(it[6], it[7], it[8])
        )
    }
}
