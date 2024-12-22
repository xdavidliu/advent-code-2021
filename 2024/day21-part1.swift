import Foundation

let numText = """
789
456
123
#0A
"""

let dirText = """
###
###
###
#^A
<v>
"""
// spaces so that A is in same position in both places
// so can put in single dict
let aLoc = (3, 2)

typealias Locs = [UInt8: (Int, Int)]
let numGrid = numText.split(separator: "\n").map{$0.asciiValues}
let dirGrid = dirText.split(separator: "\n").map{$0.asciiValues}
let locs = createDict()

func createDict() -> Locs {
    var dict: Locs = [:]
    storeToDict(numGrid, &dict)
    storeToDict(dirGrid, &dict)
    return dict
}

func storeToDict(_ grid: [[UInt8]], _ dict: inout Locs) {
    for r in grid.indices {
        for c in grid[r].indices {
            let bt = grid[r][c]
            if bt == byteOf("#") {
                continue
            }
            dict[bt] = (r, c)
        }
    }
}

func getSegment(from: UInt8, to: UInt8) -> [UInt8] {
    // also works for from == to because it appends just A at end
    let (rf, cf) = locs[from]!
    let (rt, ct) = locs[to]!
    let dr = rt - rf
    let dc = ct - cf
    var out: [UInt8] = []
    func doHoriz() {
        if dc > 0 {
            out.append(contentsOf: [UInt8](repeating: byteOf(">"), count: dc))
        }
        if dc < 0 {
            out.append(contentsOf: [UInt8](repeating: byteOf("<"), count: abs(dc)))
        }
    }
    func doVert() {
        if dr < 0 {
            out.append(contentsOf: [UInt8](repeating: byteOf("^"), count: abs(dr)))
        }
        if dr > 0 {
            out.append(contentsOf: [UInt8](repeating: byteOf("v"), count: dr))
        }
    }
    // whether dir or num, this situation would result in going thru forbidden cell
    if ct == 0 && rf == aLoc.0 {
        doVert()
        doHoriz()
    } else {
        doHoriz()
        doVert()
    }
    out.append(byteOf("A"))
    return out
}

func getPath(_ bs: [UInt8]) -> [UInt8] {
    var prev = byteOf("A")
    var out: [UInt8] = []
    for x in bs {
        let seg = getSegment(from: prev, to: x)
        out.append(contentsOf: seg)
        prev = x
    }
    return out
}

func toString(_ b: UInt8) -> String {
    return String(bytes: [b], encoding: .utf8)!
}

func fromPath(_ bs: [UInt8], _ grid: [[UInt8]]) -> [UInt8] {
    var (r, c) = aLoc
    var out: [UInt8] = []
    for bt in bs {
        switch toString(bt) {
        case "<":
            c -= 1
        case ">":
            c += 1
        case "^":
            r -= 1
        case "v":
            r += 1
        case "A":
            out.append(grid[r][c])
        default:
            fatalError("fromPath")
        }
    }
    return out
}

func getPathString(_ s: String, _ times: Int = 1) -> String {
    var bs = s.asciiValues
    for _ in 1...times {
        bs = getPath(bs)
    }
    return String(bytes: bs, encoding: .utf8)!
}

func fromPathString(_ s: String, _ times: Int = 1) -> String {
    var bs = s.asciiValues
    for i in 1...times {
        let grid = if i == 3 { numGrid } else { dirGrid }
        bs = fromPath(bs, grid)
    }
    return String(bytes: bs, encoding: .utf8)!
}

func numericPart(_ s: String) -> Int {
    let ind = s.firstIndex(of: "A")!
    return Int(s[..<ind])!
}

func bestLength(_ s: String) -> Int {
    return getPathString(s, 3).count
}

func complexity(_ s: String) -> Int {
    return bestLength(s) * numericPart(s)
}

func part1(_ lines: [String]) -> Int {
    return lines.map{complexity($0)}.reduce(0, +)
}

let lines = """
789A
968A
286A
349A
170A
""".split(separator: "\n").map(String.init)

//print(part1(lines))
// sample expect 126384

// 181666 too high

//let s = "789A"
//let s = "^^^<<A>A>AvvvA"  // 66

//let s = "968A"
//let s = "^^^AvA<^Avvv>A"  // 70

//let s = "286A"
//let s = "<^A^^Av>AvvA"  // 68

//let s = "349A"
//let s = "^A<<^A>>^AvvvA"  // 72

// this is the only one that requires attention to forbidden cells
//let s = "170A"
//let s = "^<<A^^A>vvvA>A"  // 72
//print(getPathString(s))
//print(getPathString(s, 2).count)

let p1 = 66 * 789 + 968 * 70 + 286 * 68 + 349 * 72 + 170 * 72
print(p1)
// 176650 correct.
