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

func part1() -> Int {
    let filename = "/Users/xdavidliu/input16.txt"
    let grid = getGrid(filename)
    let (nr, nc) = (grid.count, grid[0].count)
    let start = findChar(grid, "S")
    let end = findChar(grid, "E")
    let dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    var hp = MinHeap<(Int, Int, Int)>()
    hp.insert(0, (start.0, start.1, 0))  // score 0, and east is 0
    var seenScore: [Int: Int] = [:]
    let initialSeenKey = singleIndWithD(r: start.0, c: start.1, id: 0, nc: nc, nr: nr)
    seenScore[initialSeenKey] = 0
    while !hp.isEmpty {
        let (score, (r, c, id)) = hp.pop()
        let seenKey = singleIndWithD(r: r, c: c, id: id, nc: nc, nr: nr)
        if (r, c) == end {
            return score
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
            if scoreInSeen == nil || score < scoreInSeen! {
                hp.insert(score, (r, c, id))
                seenScore[seenKey] = score
            }
        }
    }
    fatalError("unexpectedly empty heap")
}

print("part 1 =", part1())  // 133584
