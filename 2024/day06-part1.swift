import Foundation

let left = (0, -1)
let right = (0, 1)
let up = (-1, 0)
let down = (1, 0)
let dirFromChar = ["<": left, ">": right, "^": up, "v": down]

func findGuard(_ grid: [[UInt8]]) -> (Int, Int) {
    let dotOrPound = ".#".asciiValues
    for r in grid.indices {
        for c in grid[r].indices {
            if !dotOrPound.contains(grid[r][c]) {
                return (r, c)
            }
        }
    }
    fatalError("guard not found")
}

func singleInd(r: Int, c: Int, nc: Int) -> Int {
    return c + r * nc
}

// not needed
func splitInd(i: Int, nc: Int) -> (Int, Int) {
    let r = i / nc
    let c = i % nc
    return (r, c)
}

// pair not hashable; otherwise this would be dict
func turnRight(dr: Int, dc: Int) -> (Int, Int) {
    let d = (dr, dc)
    if d == left {
        return up
    } else if d == up {
        return right
    } else if d == right {
        return down
    } else if d == down {
        return left
    } else {
        fatalError("turnRight invalid input")
    }
}

func nextPos(r: Int, c: Int, dr: Int, dc: Int) -> (Int, Int) {
    return (r + dr, c + dc)
}

func isInside(_ grid: [[UInt8]], r: Int, c: Int) -> Bool {
    return grid.indices.contains(r) && grid[r].indices.contains(c)
}

func part1(_ grid: [[UInt8]]) -> Int {
    var (r, c) = findGuard(grid)
    let s = String(bytes: [grid[r][c]], encoding: .utf8)!
    var (dr, dc) = dirFromChar[s]!
    let pound = Character("#").asciiValue!
    let nc = grid[0].count
    var seen: Set<Int> = [singleInd(r: r, c: c, nc: nc)]
    while true {
        while isInside(grid, r: r+dr, c: c+dc) && pound != grid[r+dr][c+dc] {
            r += dr
            c += dc
            seen.insert(singleInd(r: r, c: c, nc: nc))
        }
        if isInside(grid, r: r+dr, c: c+dc) {
            (dr, dc) = turnRight(dr: dr, dc: dc)
        } else {
            return seen.count
        }
    }
}

let grid = getLines("/Users/xdavidliu/input06.txt").map{$0.asciiValues}
print("part 1 = \(part1(grid))")  // 4973

/*
 idea: fast traversal. Record "next obstacle". Reuse part 1 code to perform traversal
 and have bool param determine whether to record seen. Return union of number
 and whether leaved. Wait, it would be infinite loop.
 
 okay so need to store set of "obstacles encountered", and return if already seen.
 probably need different function, cuz that's too different
 */
