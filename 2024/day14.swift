import Foundation

func parseLine(_ line: String) -> (Int, Int, Int, Int) {
    let reg = /p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)\n?/
    let mch = line.wholeMatch(of: reg)!
    return (Int(mch.1)!, Int(mch.2)!, Int(mch.3)!, Int(mch.4)!)
}

// https://stackoverflow.com/a/41180619/2990344
func mod(k: Int, n: Int) -> Int {
    let k = k % n  // in case k < -n
    return (k + n) % n  // for negatives
}

func evolve(p: (Int, Int), v: (Int, Int), dt: Int, n: (Int, Int)) -> (Int, Int) {
    let x = mod(k: p.0 + dt * v.0, n: n.0)
    let y = mod(k: p.1 + dt * v.1, n: n.1)
    return (x, y)
}

func quadrant(p: (Int, Int), n: (Int, Int)) -> Optional<String> {
    assert(n.0 % 2 == 1 && n.1 % 2 == 1)
    if p.0 == n.0 / 2 || p.1 == n.1 / 2 {
        return nil
    }
    var out = ""
    if p.1 < n.1 / 2 {
        out += "N"
    } else {
        out += "S"
    }
    if p.0 < n.0 / 2 {
        out += "W"
    } else {
        out += "E"
    }
    return out
}

func getCoords(_ filename: String) -> [(Int, Int, Int, Int)] {
    let lines = getLines(filename, omittingEmptySubsequences: true)
    return lines.map{parseLine($0)}
}

func evolveAll(_ coords: [(Int, Int, Int, Int)], dt: Int, n: (Int, Int)) -> [(Int, Int)] {
    var out: [(Int, Int)] = []
    for (px, py, vx, vy) in coords {
        let q = evolve(p: (px, py), v: (vx, vy), dt: dt, n: n)
        out.append(q)
    }
    return out
}

func part1(_ coords: [(Int, Int, Int, Int)], dt: Int, n: (Int, Int)) -> Int {
    var counter: [String : Int] = [:]
    let ps = evolveAll(coords, dt: dt, n: n)
    for p in ps {
        if let quad = quadrant(p: p, n: n) {
            counter[quad, default: 0] += 1
        }
    }
    assert(counter.count == 4)
    var prod = 1
    for (_, val) in counter {
        prod *= val
    }
    return prod
}

func positionsToGrid(_ ps: [(Int, Int)], _ n: (Int, Int), _ grid: inout [[UInt8]]) {
    for (x, y) in ps {
        grid[y][x] = Character("#").asciiValue!
    }
}

func hasConsecutive(_ pSet: Set<Int>, _ p: (Int, Int), nInARow: Int, nc: Int) -> Bool {
    let ind = singleInd(r: p.1, c: p.0, nc: nc)
    assert(pSet.contains(ind))
    if p.0 > nc - nInARow {
        return false
    }
    for dx in 1..<nInARow {
        if !pSet.contains(ind + dx) {
            return false
        }
    }
    return true
}

func plot(_ ps: [(Int, Int)], n: (Int, Int), grid: [[UInt8]], output: inout FileHandlerOutputStream) {
    for row in grid {
        print(String(bytes: row, encoding: .utf8)!, to: &output)
    }
}

// taken from
// https://nshipster.com/textoutputstream/#writing-output-to-a-file
struct FileHandlerOutputStream: TextOutputStream {
    private let fileHandle: FileHandle
    let encoding: String.Encoding

    init(_ fileHandle: FileHandle, encoding: String.Encoding = .utf8) {
        self.fileHandle = fileHandle
        self.encoding = encoding
    }

    mutating func write(_ string: String) {
        if let data = string.data(using: encoding) {
            fileHandle.write(data)
        }
    }
}

func clear(_ grid: inout [[UInt8]]) {
    for r in grid.indices {
        for c in grid[r].indices {
            grid[r][c] = Character(" ").asciiValue!
        }
    }
}

let dt = 100
let n = (101, 103)
let filename = "/Users/xdavidliu/input14.txt"
let coords = getCoords(filename)
//print("part 1 =", part1(coords, dt: dt, n: n)) // 224554908

func part2() -> Int {
    let url = URL(fileURLWithPath: "/Users/xdavidliu/plot.txt")
    let space = Character(" ").asciiValue!
    var grid = [[UInt8]](repeating: [UInt8](repeating: space, count: n.0), count: n.1)
    for dt in 6000...10000 {
        //    let fileHandle = try FileHandle(forWritingTo: url)
        //    var output = FileHandlerOutputStream(fileHandle)
        let ps = evolveAll(coords, dt: dt, n: n)
        let pSet = Set(ps.map{singleInd(r: $0.1, c: $0.0, nc: grid[0].count)})
        for (x, y) in ps {
            if hasConsecutive(pSet, (x, y), nInARow: 8, nc: grid[y].count) {
                return dt
            }
        }
            //    positionsToGrid(ps, n, &grid)
            //    print(tt, to: &output)
            //    plot(evolveAll(coords, dt: tt, n: n), n: n, grid: grid, output: &output)
        clear(&grid)
        //    usleep(800_000)
    }
    return -1
}

print("part 2 =", part2())  // 6644
