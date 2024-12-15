import Foundation

let robot = Character("@").asciiValue!
let box = Character("O").asciiValue!
let leftBox = Character("[").asciiValue!
let rightBox = Character("]").asciiValue!
let wall = Character("#").asciiValue!
let empty = Character(".").asciiValue!

func direction(_ ch: UInt8) -> (Int, Int) {
    return switch ch {
    case Character(">").asciiValue!: (0, 1)
    case Character("<").asciiValue!: (0, -1)
    case Character("v").asciiValue!: (1, 0)
    case Character("^").asciiValue!: (-1, 0)
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

func makeMove(_ grid: inout [[UInt8]], _ ch: UInt8, _ start: (Int, Int)) -> (Int, Int) {
    let d = direction(ch)
    var (r, c) = start
    r += d.0
    c += d.1
    while grid[r][c] == box {
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
            if grid[r][c] == box {
                out += score(r, c)
            }
        }
    }
    return out
}

func part1() {
    let filename = "/Users/xdavidliu/input15.txt"
    let text = getText(filename)
    let blocks = text.split(separator: "\n\n")
    var grid = blocks[0].split(separator: "\n").map{$0.asciiValues}
    // https://stackoverflow.com/a/24201206/2990344
    let movements = blocks[1].replacingOccurrences(of: "\n", with: "").asciiValues
    var pos = findRobot(grid)
    for ch in movements {
        pos = makeMove(&grid, ch, pos)
    }
//    plot(grid)
    print(totalScore(grid))  // 1514353
}

func foo1() {
    var x = 1
    var y = 2
    (x, y) = (y, x)
    print(x, y)
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

func foo0() {
    let filename = "/Users/xdavidliu/sample.txt"
    let text = getText(filename)
    let blocks = text.split(separator: "\n\n")
    let single = blocks[0].split(separator: "\n").map{$0.asciiValues}
    let twice = doubleWidth(single)
    plot(twice)
}

//part1()
foo0()
