import Foundation

let grid = getLines("/Users/xdavidliu/input04.txt").map(\.asciiValues)
let xmas = "XMAS".asciiValues

func check(_ grid: [[UInt8]], r0: Int, c0: Int, dr: Int, dc: Int) -> Bool {
    var r = r0
    var c = c0
    for ch in xmas {
        let valid = grid.indices.contains(r) && grid[0].indices.contains(c) && ch == grid[r][c]
        if !valid {
            return false
        }
        r += dr
        c += dc
    }
    return true
}

func part1(_ grid: [[UInt8]]) -> Int {
    // clockwise from noon
    let drs = [-1, -1, 0, 1, 1, 1, 0, -1]
    let dcs = [0, 1, 1, 1, 0, -1, -1, -1]
    var found = 0
    for r in grid.indices {
        for c in grid[r].indices {
            found += zip(drs, dcs).count(
                where: { check(grid, r0: r, c0: c, dr: $0.0, dc: $0.1) }
            )
        }
    }
    return found
}

func part2(_ grid: [[UInt8]]) -> Int {
    var found = 0
    let (m, a, s) = (xmas[1], xmas[2], xmas[3])
    for r in 1..<grid.count-1 {
        for c in 1..<grid[0].count-1 {
            let b = grid[r][c]
            if b != a { continue; }
            let nw = grid[r-1][c-1]
            let ne = grid[r-1][c+1]
            let sw = grid[r+1][c-1]
            let se = grid[r+1][c+1]
            if (m == nw && s == se || s == nw && m == se) &&
                (m == sw && s == ne || s == sw && m == ne)
            {
                found += 1
            }
        }
    }
    return found
}

print("part 1 =", part1(grid))  // 2297
print("part 2 =", part2(grid))  // 1745
