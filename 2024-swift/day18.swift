import Foundation

func emptyGrid(_ n: Int) -> [[UInt8]] {
    return makeGrid(elem: byteOf("."), nr: n, nc: n)
}

func getPoints(_ filename: String) -> [(Int, Int)] {
    var out: [(Int, Int)] = []
    for line in getLines(filename) {
        let words = line.split(separator: ",")
        let r = Int(words[1])!
        let c = Int(words[0])!
        out.append((r, c))
    }
    return out
}

func makeInsertions(_ grid: inout [[UInt8]], _ points: [(Int, Int)], toInsert: Int) {
    var insCount = 0
    for (r, c) in points {
        grid[r][c] = byteOf("#")
        insCount += 1
        if insCount == toInsert {
            break
        }
    }
}

let filename = "/Users/xdavidliu/input18.txt"  // input18
let points = getPoints(filename)
let n = 71  // 71 and 7
let toInsertPart1 = 1024  // 12 and 1024

func part1() {
    var grid = emptyGrid(n)
    makeInsertions(&grid, points, toInsert: toInsertPart1)
    print("part 1 =", bfs(grid))  // 436
}

// manually binary search this until find last toInsert that "works"
func part2() {
    var grid = emptyGrid(n)
//    print(points.count)  // 3450
    makeInsertions(&grid, points, toInsert: 2960)
    if -1 == bfs(grid) {
        print("does not work")
    } else {
        print("works")
    }
}

part1()
//part2()

let p2 = points[2960]
print("part 2 = ", p2.1, ",", p2.0, separator: "")  // 61,50

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
    return -1
}
