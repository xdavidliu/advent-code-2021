import Foundation

func emptyGrid(_ n: Int) -> [[UInt8]] {
    return makeGrid(elem: byteOf("."), nr: n, nc: n)
}

func makeInsertions(_ grid: inout [[UInt8]], _ filename: String, toInsert: Int) {
    var insCount = 0
    for line in getLines(filename) {
        let words = line.split(separator: ",")
        let r = Int(words[1])!
        let c = Int(words[0])!
        grid[r][c] = byteOf("#")
        insCount += 1
        if insCount == toInsert {
            break
        }
    }
}

let filename = "/Users/xdavidliu/input18.txt"
let n = 71
let toInsert = 1024

var grid = emptyGrid(n)
makeInsertions(&grid, filename, toInsert: toInsert)
print("part 1 =", bfs(grid))  // 436

func bfs(_ grid: [[UInt8]]) -> Int {
    var que = Queue<(Int, Int, Int)>()
    que.add((0, 0, 0))
    let nc = grid[0].count
    var seen = Set<Int>()
    seen.insert(singleInd(r: 0, c: 0, nc: nc))
    while !que.isEmpty {
        let (rq, cq, distq) = que.remove()
        for (dr, dc) in [(1,0),(-1,0),(0,1),(0,-1)] {
            let r = rq + dr
            let c = cq + dc
            if r == nc-1 && c == nc-1 {
                return 1+distq
            }
            if !isValidIndex(grid, r: r, c: c) || grid[r][c] == byteOf("#") {
                continue
            }
            let ind = singleInd(r: r, c: c, nc: nc)
            if seen.contains(ind) {
                continue
            }
            que.add((r, c, 1+distq))
            seen.insert(ind)
        }
    }
    fatalError("bfs")
}
