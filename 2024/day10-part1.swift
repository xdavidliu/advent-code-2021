import Foundation

let grid = getGrid("/Users/xdavidliu/input10.txt")
print("part 1 =", part1(grid))  // 468

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
