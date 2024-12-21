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
    let filename = "/Users/xdavidliu/input20.txt"
    let grid = getGrid(filename)
    let distFromEnd = bfs(grid, "E")
    let distFromStart = bfs(grid, "S")
    let ccs = countCheats(grid, distFromEnd: distFromEnd, distFromStart: distFromStart)
    let p1 = moreThanHundred(ccs)
    print("part 1 =", p1)  // 1518
//    let cm = countManhattan(grid, distFromEnd: distFromEnd, distFromStart: distFromStart)
//    let p2 = moreThanHundred(cm)
//    print("part 2 =", p2)  // 0
    // 1084746 too high
}

// wait start time does NOT need to be accessible from end,
// but end state DOES need to be
// so how do you do subtraction if its unreach? don't want to subtract -1
// oh, just compute total distance from S and use that, then compute actual distance
// and compare to that.
func countManhattan(_ grid: [[UInt8]], distFromEnd: [[Int]], distFromStart: [[Int]]) -> [Int: Int] {
    var cheatCount: [Int: Int] = [:]
    for rg in grid.indices {
        for cg in grid[rg].indices {
            if grid[rg][cg] == byteOf("#") || distFromStart[rg][cg] == unreach || distFromEnd[rg][cg] == unreach {
                continue
            }
            for md in 1...20 {
                for dr in max(-md, -rg)...min(md, grid.count - 1 - rg) {
                    let dc0 = md - abs(dr)
                    let r = rg + dr
                    let dcs = [dc0, -dc0].filter({grid[rg].indices.contains(cg + $0)})
                    for dc in dcs {
                        let c = cg + dc
                        if grid[r][c] == byteOf("#") || distFromStart[r][c] == unreach || distFromEnd[r][c] == unreach {
                            continue
                        }
                        let actualDiff = distFromEnd[rg][cg] - distFromEnd[r][c]
                        let saving = actualDiff - md
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
