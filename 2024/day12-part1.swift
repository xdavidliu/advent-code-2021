import Foundation

// util updated, upload to github
solve()

func solve() {
    let filename = "/Users/xdavidliu/input12.txt"
    let grid = getGrid(filename)
    let nc = grid[0].count
    var seen = Set<Int>()
    var total = 0
    for r in grid.indices {
        for c in grid.indices {
            let ind = singleInd(r: r, c: c, nc: nc)
            if !seen.contains(ind) {
                let (area, perim) = bfs(grid, r0: r, c0: c, &seen)
                total += area * perim
            }
        }
    }
    print("part 1 =", total)  // 1446042
}


func bfs(_ grid: [[UInt8]], r0: Int, c0: Int, _ seen: inout Set<Int>) -> (Int, Int) {
    var que = Queue<(Int, Int)>()
    let seenBefore = seen.count
    let nc = grid[0].count
    que.add((r0, c0))
    seen.insert(singleInd(r: r0, c: c0, nc: nc))
    var perim = 0
    while !que.isEmpty {
        let (rq, cq) = que.remove()
        for (dr, dc) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
            let r = rq + dr
            let c = cq + dc
            if !isValidIndex(grid, r: r, c: c) || grid[r][c] != grid[rq][cq] {
                perim += 1
            } else {
                let ind = singleInd(r: r, c: c, nc: nc)
                if !seen.contains(ind) {
                    seen.insert(ind)
                    que.add((r, c))
                }
            }
        }
    }
    let area = seen.count - seenBefore
    return (area, perim)
}
