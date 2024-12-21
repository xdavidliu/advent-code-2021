import Foundation

let unreach = -1

func findChar(_ grid: [[UInt8]], _ ch: String) -> (Int, Int) {
    for r in grid.indices {
        for c in grid[r].indices {
            if grid[r][c] == byteOf(ch) {
                return (r, c)
            }
        }
    }
    fatalError("findChar")
}

func bfs(_ grid: [[UInt8]], _ start: String) -> [[Int]] {
    let (r0, c0) = findChar(grid, start)
    var que = Queue<(Int, Int)>()
    let nr = grid.count
    let nc = grid[0].count
    var dist = [[Int]](repeating: [Int](repeating: unreach, count: nc), count: nr)
    que.add((r0, c0))
    dist[r0][c0] = 0
    while !que.isEmpty {
        let (rq, cq) = que.remove()
        for (dr, dc) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
            let r = rq + dr
            let c = cq + dc
            if grid[r][c] != byteOf("#") && dist[r][c] == unreach {
                dist[r][c] = 1 + dist[rq][cq]
                que.add((r, c))
            }
        }
    }
    return dist
}

func countCheats(_ grid: [[UInt8]], distFromEnd: [[Int]], distFromStart: [[Int]]) -> [Int: Int] {
    var cheatCount: [Int: Int] = [:]
    for rg in grid.indices {
        for cg in grid[rg].indices {
            if grid[rg][cg] != byteOf("#") {
                continue
            }
            var ds: [Int] = []
            for (dr, dc) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                let r = rg + dr
                let c = cg + dc
                if isValidIndex(grid, r: r, c: c) && grid[r][c] != byteOf("#")
                    && distFromStart[r][c] != unreach {
                    let z = distFromEnd[r][c]
                    if z != unreach {
                        ds.append(z)
                    }
                }
            }
            if ds.count >= 2 {
                let cc = ds.max()! - ds.min()! - 2
                if cc > 0 {
                    cheatCount[cc, default: 0] += 1
                }
            }
        }
    }
    return cheatCount
}

func moreThanHundred(_ ccs: [Int: Int]) -> Int {
    var p1 = 0
    for (k, c) in ccs {
        if k >= 100 {
            p1 += c
        }
    }
    return p1
}

func solve() {
    let filename = "/Users/xdavidliu/sample.txt"
    let grid = getGrid(filename)
    let distFromEnd = bfs(grid, "E")
    let distFromStart = bfs(grid, "S")
    let ccs = countCheats(grid, distFromEnd: distFromEnd, distFromStart: distFromStart)
    let p1 = moreThanHundred(ccs)
    print("part 1 =", p1)  // 1518
    let cm = countManhattan(grid, distFromEnd: distFromEnd, distFromStart: distFromStart)
    let p2 = moreThanHundred(cm)
    print("part 2 =", p2)  // 0
    // 1084746 too high, 1196351 even higher
    for (k, c) in cm {
        if k >= 50 {
            print(c, k)
        }
    }
}

// wait start time does NOT need to be accessible from end,
// but end state DOES need to be
// so how do you do subtraction if its unreach? don't want to subtract -1
// oh, just compute total distance from S and use that, then compute actual distance
// and compare to that.
func countManhattan(_ grid: [[UInt8]], distFromEnd: [[Int]], distFromStart: [[Int]]) -> [Int: Int] {
    let (r0, c0) = findChar(grid, "S")
    let cheatlessDist = distFromEnd[r0][c0]
    var cheatCount: [Int: Int] = [:]
    for rg in grid.indices {
        for cg in grid[rg].indices {
            // note deliberate do not check distFromEnd unreach here, since you can cheat from here
            // and can make it to end
            if grid[rg][cg] == byteOf("#") || distFromStart[rg][cg] == unreach {
                continue
            }
            for md in 1...20 {
                for dr in max(-md, -rg)...min(md, grid.count - 1 - rg) {
                    let dc0 = md - abs(dr)
                    let r = rg + dr
                    let dcs = [dc0, -dc0].filter({grid[rg].indices.contains(cg + $0)})
                    for dc in dcs {
                        let c = cg + dc
                        // deliberately do not check distFromStart == unreach here, for
                        // similar reason as above
                        if grid[r][c] == byteOf("#") || distFromEnd[r][c] == unreach {
                            continue
                        }
                        let cheatfulDist = distFromStart[rg][cg] + md + distFromEnd[r][c]
                        let saving = cheatfulDist - cheatlessDist
                        if saving > 0 {
                            cheatCount[saving, default: 0] += 1
                        }
                    }
                }
            }
        }
    }
    return cheatCount
}

solve()

/*
 4 90
 62 58
 70 52    31
 55 72
 54 62
 187 68
 57 84
 40 70
 120 56
 51 64
 11 88
 32 78
 48 66
 3 92
 69 54      29
 8 86
 106 80
 34 74
 21 82
 76 60
 34 76
 70 50     32

 */
