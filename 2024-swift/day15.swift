import Foundation

let robot = Character("@").asciiValue!
let box = Character("O").asciiValue!
let leftBox = Character("[").asciiValue!
let rightBox = Character("]").asciiValue!
let wall = Character("#").asciiValue!
let empty = Character(".").asciiValue!
let goLeft = Character("<").asciiValue!
let goRight = Character(">").asciiValue!
let goUp = Character("^").asciiValue!
let goDown = Character("v").asciiValue!

func isBox(_ ch: UInt8) -> Bool {
    return ch == box || ch == leftBox || ch == rightBox
}

func isBoxStart(_ ch: UInt8) -> Bool {
    return ch == box || ch == leftBox
}

func direction(_ ch: UInt8) -> (Int, Int) {
    return switch ch {
    case goRight: (0, 1)
    case goLeft: (0, -1)
    case goDown: (1, 0)
    case goUp: (-1, 0)
    default: fatalError("delta")
    }
}

func findRobot(_ grid: [[UInt8]]) -> (Int, Int) {
    for r in grid.indices {
        for c in grid[r].indices {
            if grid[r][c] == robot {
                return (r, c)
            }
        }
    }
    fatalError("findRobot")
}

func addIfNotThere(_ layer: inout [(Int, Int)], _ toAdd: (Int, Int)) {
    if layer.isEmpty || layer.last! != toAdd {
        layer.append(toAdd)
    }
}

func makeMoveTwo(_ grid: inout [[UInt8]], _ ch: UInt8, _ start: (Int, Int)) -> (Int, Int) {
    if ch == goLeft || ch == goRight {
        return makeMoveOne(&grid, ch, start)
    }
    let dr = if ch == goUp { -1 } else { 1 }
    var stk = [[start]]
    while true {
        var nextLayer: [(Int, Int)] = []
        for (r, c) in stk.last! {
            let next = grid[r+dr][c]
            let curr = grid[r][c]
            let nextAdj = grid[r+dr][c+1]
            if next == wall || curr == leftBox && nextAdj == wall {
                return start
            }
            if next == rightBox {
                addIfNotThere(&nextLayer, (r+dr, c-1))
            } else if next == leftBox {
                addIfNotThere(&nextLayer, (r+dr, c))
            }
            if curr == leftBox && nextAdj == leftBox {
                addIfNotThere(&nextLayer, (r+dr, c+1))
            }
        }
        if nextLayer.isEmpty {
            break
        } else {
            stk.append(nextLayer)
        }
    }
    while let layer = stk.popLast() {
        for (r, c) in layer {
            let end = if grid[r][c] == robot { 0 } else { 1 }
            for dc in 0...end {
                grid[r+dr][c+dc] = grid[r][c+dc]
                grid[r][c+dc] = empty
            }
        }
    }
    return (start.0 + dr, start.1)
}

func makeMoveOne(_ grid: inout [[UInt8]], _ ch: UInt8, _ start: (Int, Int)) -> (Int, Int) {
    let d = direction(ch)
    var (r, c) = start
    r += d.0
    c += d.1
    while isBox(grid[r][c]) {
        r += d.0
        c += d.1
    }
    if grid[r][c] == wall {
        return start
    }
    assert(grid[r][c] == empty)
    while (r, c) != start {
        let rd = r - d.0
        let cd = c - d.1
        grid[r][c] = grid[rd][cd]
        r = rd
        c = cd
    }
    grid[r][c] = empty
    return (r + d.0, c + d.1)
}

func score(_ r: Int, _ c: Int) -> Int {
    return 100 * r + c
}

func totalScore(_ grid: [[UInt8]]) -> Int {
    var out = 0
    for r in grid.indices {
        for c in grid[r].indices {
            if isBoxStart(grid[r][c]) {
                out += score(r, c)
            }
        }
    }
    return out
}

func solve() {
    let filename = "/Users/xdavidliu/input15.txt"
    let text = getText(filename)
    let blocks = text.split(separator: "\n\n")
    var grid = blocks[0].split(separator: "\n").map{$0.asciiValues}
    var twice = doubleWidth(grid)
    let movements = blocks[1].replacingOccurrences(of: "\n", with: "").asciiValues
    let start = findRobot(grid)
    let startTwo = findRobot(twice)
    var posOne = start
    var posTwo = startTwo
    for ch in movements {
        posOne = makeMoveOne(&grid, ch, posOne)
        posTwo = makeMoveTwo(&twice, ch, posTwo)
    }
    print("part 1 =", totalScore(grid))  // 1514353
    print("part 2 =", totalScore(twice))  // 1533076
}

func doubleChar(_ ch: UInt8) -> (UInt8, UInt8) {
    return switch ch {
    case wall: (wall, wall)
    case box: (leftBox, rightBox)
    case empty: (empty, empty)
    case robot: (robot, empty)
    default: fatalError("doubleChar")
    }
}

func doubleWidth(_ single: [[UInt8]]) -> [[UInt8]] {
    let nr = single.count
    let nc = single[0].count
    var out = [[UInt8]](repeating: [UInt8](repeating: empty, count: 2 * nc), count: nr)
    for r in single.indices {
        for c in single[r].indices {
            let chs = doubleChar(single[r][c])
            (out[r][2*c], out[r][2*c+1]) = chs
        }
    }
    return out
}

solve()
