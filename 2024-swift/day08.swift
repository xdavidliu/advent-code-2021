import Foundation

let grid = getGrid("/Users/xdavidliu/input08.txt")
let posDict = computePosDict(grid)
let p1 = countNodes(grid, posDict, getTwiceOver)
print("part 1 = \(p1)")  // 423
let p2 = countNodes(grid, posDict, getAllInLine)
print("part 2 = \(p2)")  // 1287

func computePosDict(_ grid: [[UInt8]]) -> [UInt8 : [(Int, Int)]] {
    var posDict: [UInt8 : [(Int, Int)]] = [:]
    for r in grid.indices {
        for c in grid.indices {
            let g = grid[r][c]
            let ch = Character(UnicodeScalar(g))
            if ch.isLetter || ch.isNumber {
                posDict[g, default: []].append((r, c))
            }
        }
    }
    return posDict
}

// https://stackoverflow.com/a/33717462/2990344
// _ is necessary for some reason
typealias CollectFunc
= (_ r0: Int, _ c0: Int, _ r1: Int, _ c1: Int, _ grid: [[UInt8]]) -> [(Int, Int)]

func countNodes(_ grid: [[UInt8]], _ posDict: [UInt8 : [(Int, Int)]],
                _ collect: CollectFunc) -> Int {
    var found: Set<Int> = []
    let nc = grid[0].count
    for (_, points) in posDict {
        for i in points.indices {
            for k in points.indices {
                if i == k {
                    continue
                }
                let pi = points[i]
                let pk = points[k]
                let toAdd = collect(pi.0, pi.1, pk.0, pk.1, grid)
                for p in toAdd {
                    found.insert(singleInd(r: p.0, c: p.1, nc: nc))
                }
            }
        }
    }
    return found.count
}

func getTwiceOver(_ r0: Int, _ c0: Int, _ r1: Int, _ c1: Int, _ grid: [[UInt8]]) -> [(Int, Int)] {
    let dr = r1 - r0
    let dc = c1 - c0
    let r = r0 + 2 * dr
    let c = c0 + 2 * dc
    if grid.indices.contains(r) && grid[0].indices.contains(c) {
        return [(r, c)]
    } else {
        return []
    }
}

func getAllInLine(_ r0: Int, _ c0: Int, _ r1: Int, _ c1: Int, _ grid: [[UInt8]]) -> [(Int, Int)] {
    var dr = r1 - r0
    var dc = c1 - c0
    let g = gcd(abs(dr), abs(dc))
    dr /= g
    dc /= g
    var out: [(Int, Int)] = []
    var r = r0
    var c = c0
    while grid.indices.contains(r) && grid[0].indices.contains(c) {
        out.append((r, c))
        r += dr
        c += dc
    }
    return out
}
