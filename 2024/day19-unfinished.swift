import Foundation

var uniqueCount = 0

typealias Children = [Optional<Tree>]

func uniqueBytesToInt(_ bytes: [[UInt8]], _ unique: [UInt8]) -> [[Int]] {
    return bytes.map{ row -> [Int] in
        row.map{ ch -> Int in
            getInd(ch, unique)
        }
    }
}

func getInd(_ ch: UInt8, _ unique: [UInt8]) -> Int {
    return unique.firstIndex(of: ch)!
}

func getUnique(_ words: [[UInt8]]) -> [UInt8] {
    var unique: [UInt8] = []
    for w in words {
        for ch in w {
            if !unique.contains(ch) {
                unique.append(ch)
            }
        }
    }
    return unique
}

func readProblem(_ filename: String) -> ([[Int]], [[Int]]) {
    let text = getText(filename)
    let blocks = text.split(separator: "\n\n")
    let towels = blocks[0].split(separator: ", ").map{$0.asciiValues}
    let designs = blocks[1].split(separator: "\n").map{$0.asciiValues}
    let unique = getUnique(towels)
    uniqueCount = unique.count
    let towelsInt = uniqueBytesToInt(towels, unique)
    let designsInt = uniqueBytesToInt(designs, unique)
    return (towelsInt, designsInt)
}

struct Tree {
    var child: Children = Children(repeating: nil, count: uniqueCount)
    var complete: Bool = false
}

func insertSuffix(_ trees: inout Children, _ start: Int, _ towel: [Int]) {
    if start >= towel.count {
        return
    }
    let i = towel[start]
    if trees[i] == nil {
        trees[i] = Tree()
    }
    if start == towel.count - 1 {
        trees[i]!.complete = true
    } else {
        insertSuffix(&trees[i]!.child, start+1, towel)
    }
}

func populateRoots(_ towels: [[Int]]) -> Children {
    var roots = Children(repeating: nil, count: uniqueCount)
    for t in towels {
        insertSuffix(&roots, 0, t)
    }
    return roots
}

func solve() {
    let filename = "/Users/xdavidliu/temp.txt"
    let (towels, designs) = readProblem(filename)
    let roots = populateRoots(towels)
    var p1 = 0
    for d in designs {
        if matchwhole(roots, d) {
            p1 += 1
        }
    }
    print(p1)  // 400 too high
}

// problem is when start == design.count, you don't know if it's complete
// need to check that it's count - 1 and it's complete
func matchsuffix(roots: Children, trees: Children, _ start: Int, _ design: [Int]) -> Bool {
    let ti = trees[design[start]]
    print(start)
    if ti == nil {
        return false
    } else if start == design.count - 1 {
        return ti!.complete
    } else {
        let more = matchsuffix(roots: roots, trees: ti!.child, start+1, design)
        return more || ti!.complete && matchsuffix(roots: roots, trees: roots, start+1, design)
    }
}

func matchwhole(_ roots: Children, _ design: [Int]) -> Bool {
    return matchsuffix(roots: roots, trees: roots, 0, design)
}

solve()

// Wait, matching needs roots and trees both. give start and design, and trees,
// let t = trees[design[start]], then if it is complete, then ... oh you still need to branch
// off, and try the one that is NOT complete. Need OR to short circuit. Start over
// by going to root.
