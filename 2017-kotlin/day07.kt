import java.io.File

data class Program(val name: String, val weight: Int, val above: List<String>)

fun readPrograms(file: String) = File(file).bufferedReader().readLines()
    .map { parseProgram(it) }

data class Node(val name: String, val nodeWeight: Int, var treeWeight: Int,
                var children: List<Node>, var parent: Node?)

fun createTree(programs: List<Program>): Node {
    val nodes = mutableMapOf<String, Node>()
    for (p in programs) {
        nodes[p.name] = Node(
            name = p.name,
            nodeWeight = p.weight,
            treeWeight = p.weight,
            children = emptyList(),
            parent = null
        )
    }
    for (p in programs) {
        val nd = nodes[p.name]!!
        nd.children = p.above.map { nodes[it]!! }
        for (ch in nd.children) {
            ch.parent = nd
        }
    }
    var nd = nodes.values.first()
    while (nd.parent != null) {
        nd = nd.parent!!
    }
    computeTreeWeight(nd)
    return nd
}

fun computeTreeWeight(nd: Node) {
    for (ch in nd.children) {
        computeTreeWeight(ch)
        nd.treeWeight += ch.treeWeight
    }
}

fun findRepeatedValue(nums: List<Int>): Int {
    val seen = mutableSetOf<Int>()
    for (x in nums) {
        if (!seen.add(x)) {
            return x
        }
    }
    error("no repeated")
}

fun part2(root: Node): Int {
    var nd = root
    var diffNeeded: Int? = null
    while (true) {
        val repeat = findRepeatedValue(nd.children.map { it.treeWeight })
        val thisDiff = nd.children.size * repeat + nd.nodeWeight - nd.treeWeight
        if (diffNeeded == null) {
            diffNeeded = thisDiff  // first one guaranteed nonzero
        } else if (thisDiff == 0) {
            return nd.nodeWeight + diffNeeded
        }
        nd = nd.children.find { it.treeWeight == repeat - diffNeeded }!!
    }
}

fun main() {
    val programs = readPrograms("/home/xdavidliu/Documents/aoc/input07.txt")
    val root = createTree(programs)
    println("part 1 = ${root.name}")  // vgzejbd
    val p2 = part2(root)
    println("part 2 = $p2")  // 1226
}

val lhsReg = Regex("""(\w+) \((\d+)\)""")
val rhsReg = Regex(""".* -> (.+)""")

fun parseProgram(line: String): Program {
    val lhs = lhsReg.matchAt(line, 0)!!.groupValues
    return Program (
        name = lhs[1],
        weight = lhs[2].toInt(),
        above = rhsReg.matchEntire(line)
            ?.groupValues
            ?.let { it[1].split(", ") }
            ?: emptyList()
    )
}
