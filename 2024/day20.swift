import Foundation

let unreach = -1

// this is slow, can be improved. Slowest part is the double loop over onPath.
// lots of those points are not actually on the path. To speed up, create parent tree
// when BFS so you directly have the points on the path. That I believe is much fewer
// than all points I currently call onPath which just have != unreach for both fromStart
// and fromEnd.

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
    let cm = countManhattan(grid, distFromEnd: distFromEnd, distFromStart: distFromStart)
    let p2 = moreThanHundred(cm)
    print("part 2 =", p2)  // 1032257
//    for (k, c) in cm {
//        if k >= 50 {
//            print(c, k)
//        }
//    }
}

// no, that's not the path!
func countManhattan(_ grid: [[UInt8]], distFromEnd: [[Int]], distFromStart: [[Int]]) -> [Int: Int] {
    var cheatCount: [Int: Int] = [:]
    var onPath: [(Int, Int)] = []
    for r in grid.indices {
        for c in grid[r].indices {
            if distFromEnd[r][c] != unreach && distFromStart[r][c] != unreach {
                onPath.append((r, c))
            }
        }
    }
    for (r1, c1) in onPath {
        for (r2, c2) in onPath {
            if r1 == r2 && c1 == c2 {
                continue
            }
            let diff = distFromEnd[r1][c1] - distFromEnd[r2][c2]
            if diff < 0 {
                continue
            }
            let md = abs(r1 - r2) + abs(c1 - c2)
            if md > 20 {
                continue
            }
            if md < diff {
                cheatCount[diff - md, default: 0] += 1
            }
        }
    }
    return cheatCount
}

solve()
