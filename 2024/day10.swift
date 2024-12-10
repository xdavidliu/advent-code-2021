import Foundation

let grid = getGrid("/Users/xdavidliu/input10.txt")
print("part 1 =", part1(grid))  // 468
print("part 2 =", part2(grid))  // 966

func part1(_ grid: [[UInt8]]) -> Int {
    var p1 = 0
    for r in grid.indices {
        for c in grid[r].indices {
            if grid[r][c] == Character("0").asciiValue! {
                p1 += bfs(grid, r0: r, c0: c)
            }
        }
    }
    return p1
}

func part2(_ grid: [[UInt8]]) -> Int {
    var valTable = [[(Int, Int)]](repeating: [], count: 10)
    for r in grid.indices {
        for c in grid[r].indices {
            let v = grid[r][c] - Character("0").asciiValue!
            valTable[Int(v)].append((r, c))
        }
    }
    let nr = grid.count
    let nc = grid[0].count
    var countGrid = [[Int]](repeating: [Int](repeating: 0, count: nc), count: nr)
    for (r, c) in valTable[9] {
        countGrid[r][c] = 1
    }
    for i in (0..<9).reversed() {
        for (r0, c0) in valTable[i] {
            let v0 = grid[r0][c0]
            for (dr, dc) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
                let r = r0 + dr
                let c = c0 + dc
                if grid.indices.contains(r) && grid[r].indices.contains(c)
                    && grid[r][c] == 1 + v0 {
                    countGrid[r0][c0] += countGrid[r][c]
                }
            }
        }
    }
    var p2 = 0
    for (r, c) in valTable[0] {
        p2 += countGrid[r][c]
    }
    return p2
}

func bfs(_ grid: [[UInt8]], r0: Int, c0: Int) -> Int {
    var q = Queue<(Int, Int)>()
    let nc = grid[0].count
    var seen = Set<Int>()
    var nineSeen = Set<Int>()
    q.add((r0, c0))
    while !q.isEmpty {
        let next = q.remove()
        let qVal = grid[next.0][next.1]
        for (dr, dc) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
            let r = next.0 + dr
            let c = next.1 + dc
            let ind = singleInd(r: r, c: c, nc: nc)
            if grid.indices.contains(r) && grid[r].indices.contains(c)
                && grid[r][c] == 1 + qVal && !seen.contains(ind) {
                seen.insert(ind)
                if grid[r][c] == Character("9").asciiValue! {
                    nineSeen.insert(ind)
                } else {
                    q.add((r, c))
                }
            }
        }
    }
    return nineSeen.count
}
