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

func plot(_ ps: [(Int, Int)], n: (Int, Int), output: inout FileHandlerOutputStream) {
    let space = Character(" ").asciiValue!
    var grid = [[UInt8]](repeating: [UInt8](repeating: space, count: n.0), count: n.1)
    for (x, y) in ps {
        grid[y][x] = Character("#").asciiValue!
    }
    for row in grid {
        print(String(bytes: row, encoding: .utf8)!, to: &output)
    }
}

let dt = 100
let n = (101, 103)
let filename = "/Users/xdavidliu/input14.txt"
let coords = getCoords(filename)
//print("part 1 =", part1(coords, dt: dt, n: n)) // 224554908

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

let url = URL(fileURLWithPath: "/Users/xdavidliu/plot.txt")

for tt in 52...52 {
    let fileHandle = try FileHandle(forWritingTo: url)
    var output = FileHandlerOutputStream(fileHandle)
    print(tt, to: &output)
    plot(evolveAll(coords, dt: tt, n: n), n: n, output: &output)
    usleep(800_000)
}
