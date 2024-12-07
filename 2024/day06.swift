import Foundation

let left = (0, -1)
let right = (0, 1)
let up = (-1, 0)
let down = (1, 0)
let dirFromChar = ["<": left, ">": right, "^": up, "v": down]
let poundChar = Character("#").asciiValue!
let dotChar = Character(".").asciiValue!

func findGuard(_ grid: [[UInt8]]) -> (Int, Int) {
    for r in grid.indices {
        for c in grid[r].indices {
            let ch = grid[r][c]
            if ch != poundChar && ch != dotChar {
                return (r, c)
            }
        }
    }
    fatalError("guard not found")
}

func singleInd(r: Int, c: Int, nc: Int) -> Int {
    return c + r * nc
}

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

func solve(_ grid: [[UInt8]], part: Int) -> (Set<Int>, Bool) {
    var (r, c) = findGuard(grid)
    let s = String(bytes: [grid[r][c]], encoding: .utf8)!
    var (dr, dc) = dirFromChar[s]!
    let nc = grid[0].count
    var seen: Set<Int> = [singleInd(r: r, c: c, nc: nc)]
    var turnSeen: Set<String> = []
    while true {
        while isInside(grid, r: r+dr, c: c+dc)
                && poundChar != grid[r+dr][c+dc]
        {
            r += dr
            c += dc
            if part == 1 {
                seen.insert(singleInd(r: r, c: c, nc: nc))
            }
        }
        if isInside(grid, r: r+dr, c: c+dc) {
            if part == 2 {
                // need to do this because tuple not hashable
                let key = String(format: "%d %d %d %d", r, c, dr, dc)
                // cannot use queue of 5 because some loops not simple rectangles.
                // for example the (8, 3) obstacle loop in example
                let (inserted, _) = turnSeen.insert(key)
                if !inserted {
                    return ([], true)
                }
            }
            (dr, dc) = turnRight(dr: dr, dc: dc)
        } else {
            return (seen, false)
        }
    }
}

// https://stackoverflow.com/a/45641201/2990344
func tryObstacle(_ grid: inout [[UInt8]], r: Int, c: Int) -> Bool {
    if grid[r][c] != dotChar {
        // to avoid obstacle at start
        return false
    }
    grid[r][c] = poundChar
    let (_, isLoop) = solve(grid, part: 2)
    grid[r][c] = dotChar
    
    return isLoop
}

func solvePart2(_ grid: inout [[UInt8]], _ foundPart1: Set<Int>) -> Int {
    let nc = grid[0].count
    var p2 = 0
    var done = 0
    for i in foundPart1 {
        if 0 == done % 200 {
            print(String(format: "done %d / %d", done, foundPart1.count))
        }
        let (r, c) = splitInd(i: i, nc: nc)
        if tryObstacle(&grid, r: r, c: c) {
            p2 += 1
        }
        done += 1
    }
    return p2
}

var grid = getLines("/Users/xdavidliu/input06.txt").map{$0.asciiValues}
let (setP1, _) = solve(grid, part: 1)
let p2 = solvePart2(&grid, setP1)
print("part 1 = \(setP1.count)")  // 4973
print("part 2 = \(p2)")  // 1482


/*
 okay so need to store set of "obstacles encountered", and return if already seen.
 probably need different function, and not reuse part 1, cuz that's too different
 
 oh for trying various obstacles, only need to try those along the path. ie seen from part1
 cuz off the path will not effect path. Need to recompute the "next obstacle" values
 in a + shape around the new obstacle. Can store in two temp 1d arrs
 
 toRight[r][c] gives number of steps before you will stop at a #. Or -1 if it will
 exit. Can fill this out using same Inc and Check technique as in the 2022 day 9 or
 whatev.
 
 so have toRight, toLeft, toUp, toDown. Now put obstacle at r, c. Need to update
 toRight[r][:], toLeft[r][:], toUp[:][c] and toDown[:][c]. So save those to temp.
 
 but wait, actually may want to try trying the seen in order. That way you only
 shift 1 pos... ah but you still need to change entire r if go down, etc. So just
 copy whole thing over...
 
 oh and when you do toLeft, you only need to modify until you encounter another #,
 because beyond that it doesn't change anything.
 
 So once you update the toX tables with new obstacle, you can run it. To decide if
 there's loop, keep track of last 4 corners turning at. I believe if the next one
 equals the first of the four, you're in a loop. I wonder if direction matters.
 Like if you have
 
  #
 #
 
 meh if it's within 4, direction definitely does not matter.
 
 Okay and if it leaves (-1) it's def not a loop.
 
 or just reuse existing code. See how long it takes with 4000. Maybe try sample first.
 if too slow, then implement the above.
 
 */
