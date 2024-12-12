import Foundation

let deltas = [(1, 0), (-1, 0), (0, 1), (0, -1)]

solve()

func singleIndWithD(r: Int, c: Int, id: Int, nc: Int, nr: Int) -> Int {
    return id * nr * nc + r * nc + c
}

func unwrapSingleInd(i: Int, nr: Int, nc: Int) -> (Int, Int, Int) {
    let nrc = nr * nc
    let id = i / nrc
    let k = i % nrc
    let r = k / nc
    return (r, k % nc, id)
}

func solve() {
    let filename = "/Users/xdavidliu/input12.txt"
    let grid = getGrid(filename)
    let nc = grid[0].count
    let nr = grid.count
    var seen = Set<Int>()
    var total = 0
    var sideTotal = 0
    for r in grid.indices {
        for c in grid.indices {
            let ind = singleInd(r: r, c: c, nc: nc)
            if !seen.contains(ind) {
                let (area, perimSeen) = bfs(grid, r0: r, c0: c, &seen)
                total += area * perimSeen.count
                sideTotal += area * countSides(perimSeen, nr: nr, nc: nc)
            }
        }
    }
    print("part 1 =", total)  // 1446042
    print("part 2 =", sideTotal)  // 902742
}

func orthogonalDirections(_ delta: (Int, Int)) -> [(Int, Int)] {
    if delta.0 == 0 {
        return [(1, 0), (-1, 0)]
    } else {
        return [(0, 1), (0, -1)]
    }
}

func countSides(_ perimSeen: Set<Int>, nr: Int, nc: Int) -> Int {
    var seenAgain = Set<Int>()
    var sideCount = 0
    for ind0 in perimSeen {
        if seenAgain.contains(ind0) {
            continue
        }
        seenAgain.insert(ind0)
        sideCount += 1
        let (r0, c0, id) = unwrapSingleInd(i: ind0, nr: nr, nc: nc)
        for orthDelta in orthogonalDirections(deltas[id]) {
            var (r, c) = (r0, c0)
            while true {
                r += orthDelta.0
                c += orthDelta.1
                let ind = singleIndWithD(r: r, c: c, id: id, nc: nc, nr: nr)
                if perimSeen.contains(ind) {
                    seenAgain.insert(ind)
                } else {
                    break
                }
            }
        }
    }
    return sideCount
}

func bfs(_ grid: [[UInt8]], r0: Int, c0: Int, _ seen: inout Set<Int>) -> (Int, Set<Int>) {
    var que = Queue<(Int, Int)>()
    let seenBefore = seen.count
    var perimSeen = Set<Int>()
    let nc = grid[0].count
    let nr = grid.count
    que.add((r0, c0))
    seen.insert(singleInd(r: r0, c: c0, nc: nc))
    while !que.isEmpty {
        let (rq, cq) = que.remove()
        for id in deltas.indices {
            let r = rq + deltas[id].0
            let c = cq + deltas[id].1
            if !isValidIndex(grid, r: r, c: c) || grid[r][c] != grid[rq][cq] {
                perimSeen.insert(singleIndWithD(r: rq, c: cq, id: id, nc: nc, nr: nr))
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
    return (area, perimSeen)
}
