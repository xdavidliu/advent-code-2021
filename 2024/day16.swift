import Foundation

func findChar(_ grid: [[UInt8]], _ ch: String) -> (Int, Int) {
    for r in grid.indices {
        for c in grid[r].indices {
            if grid[r][c] == Character(ch).asciiValue! {
                return (r, c)
            }
        }
    }
    fatalError("findStart")
}

func solve() {
    let filename = "/Users/xdavidliu/input16.txt"
    let grid = getGrid(filename)
    let (nr, nc) = (grid.count, grid[0].count)
    var auxGrid = [[UInt8]](repeating: [UInt8](repeating: 0, count: nc), count: nr)
    let isPartOfBest: UInt8 = 1
    let start = findChar(grid, "S")
    let end = findChar(grid, "E")
    var lowestEnd = -1
    let dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    var hp = MinHeap<(Int, Int, Int, [Int])>()
    hp.insert(0, (start.0, start.1, 0, [singleInd(r: start.0, c: start.1, nc: nc)]))
    // score 0, and east is 0
    var seenScore: [Int: Int] = [:]
    let initialSeenKey = singleIndWithD(r: start.0, c: start.1, id: 0, nc: nc, nr: nr)
    seenScore[initialSeenKey] = 0
    while !hp.isEmpty {
        let (score, (r, c, id, path)) = hp.pop()
        let seenKey = singleIndWithD(r: r, c: c, id: id, nc: nc, nr: nr)
        if lowestEnd != -1 && score > lowestEnd {
            break
        }
        if (r, c) == end {
            if lowestEnd == score || lowestEnd == -1 {
                for ind in path {
                    let (rp, cp) = splitInd(i: ind, nc: nc)
                    auxGrid[rp][cp] = isPartOfBest
                }
                lowestEnd = score
            }
        }
        // it's okay to put this afterwards for end because a suboptimal score for
        // end will never be seen first; the below is just to clean up garbage in the heap
        if score > seenScore[seenKey]! {
            continue
        }
        let (dr, dc) = dirs[id]
        let (drLeft, dcLeft) = (-dc, dr)
        let idLeft = (id - 1 + 4) % 4
        let (drRight, dcRight) = (dc, -dr)
        let idRight = (id + 1) % 4
        let neighbors = [(r+dr, c+dc, id, score+1),
                         (r+drLeft, c+dcLeft, idLeft, score+1000+1),
                         (r+drRight, c+dcRight, idRight, score+1000+1)]
        for (r, c, id, score) in neighbors {
            if grid[r][c] == Character("#").asciiValue! {
                continue
            }
            let seenKey = singleIndWithD(r: r, c: c, id: id, nc: nc, nr: nr)
            let scoreInSeen = seenScore[seenKey]
            if scoreInSeen == nil || score <= scoreInSeen! {
                var nextPath = path  // arrays get copied
                nextPath.append(singleInd(r: r, c: c, nc: nc))
                hp.insert(score, (r, c, id, nextPath))
                seenScore[seenKey] = score
            }
        }
    }
    print("part 1 =", lowestEnd)  // 133584
    var p2 = 0
    for r in auxGrid.indices {
        for c in auxGrid[r].indices {
            if auxGrid[r][c] == isPartOfBest {
                p2 += 1
            }
        }
    }
    print("part 2 =", p2)  // 622
}

solve()
