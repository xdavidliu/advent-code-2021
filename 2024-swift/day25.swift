import Foundation

let present = byteOf("#")

func block2grid<T>(_ blk: T) -> [[UInt8]] where T: StringProtocol {
    return blk.split(separator: "\n").map{$0.asciiValues}
}

func readGrids(_ filename: String) -> [[[UInt8]]] {
    getText(filename).split(separator: "\n\n").map(block2grid(_:))
}

func firstFromAbove(_ g: [[UInt8]], _ c: Int) -> Int {
    return firstFrom(g, col: c, start: 0, incr: 1)
}

func firstFromBelow(_ g: [[UInt8]], _ c: Int) -> Int {
    return firstFrom(g, col: c, start: g.count-1, incr: -1)
}

func firstFrom(_ g: [[UInt8]], col: Int, start: Int, incr: Int) -> Int {
    var i = start
    while g[i][col] != present {
        i += incr
    }
    return i
}

func gridTypes(_ grids: [[[UInt8]]]) -> ([[Int]], [[Int]]) {
    var keys: [[Int]] = []
    var locks: [[Int]] = []
    for g in grids {
        let nr = g.count
        let isLock = g[0][0] == present
        var cols: [Int] = []
        for c in 0..<g[0].count {
            let ht = if isLock {
                firstFromBelow(g, c)
            } else {
                nr - 1 - firstFromAbove(g, c)
            }
            cols.append(ht)
        }
        if isLock {
            locks.append(cols)
        } else {
            keys.append(cols)
        }
    }
    return (locks, keys)
}

func isFit(_ total: Int, lock: [Int], key: [Int]) -> Bool {
    for c in 0..<lock.count {
        if lock[c] + key[c] > total {
            return false
        }
    }
    return true
}

let grids = readGrids("/Users/xdavidliu/input25.txt")
let total = grids[0].count - 2
let (locks, keys) = gridTypes(grids)
var p1 = 0
for l in locks {
    for k in keys {
        if isFit(total, lock: l, key: k) {
            p1 += 1
        }
    }
}
print(p1)  // 3320
