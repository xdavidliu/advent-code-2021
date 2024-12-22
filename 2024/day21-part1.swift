import Foundation

let allDirs = "><^vA".asciiValues
let allNums = "0123456789A".asciiValues

func toString(_ x: UInt8) -> String {
    return String(bytes: [x], encoding: .utf8)!
}

func toString(_ x: UInt8, _ y: UInt8) -> String {
    return String(bytes: [x, y], encoding: .utf8)!
}

func padPos(_ x: UInt8) -> (Int, Int) {
    return switch toString(x) {
    case "<": (1, 0)
    case "^": (0, 1)
    case "A": (0, 2)
    case "v": (1, 1)
    case ">": (1, 2)
    case "0": (0, 1)
    case "1": (-1, 0)  // make negative so A same position
    case "2": (-1, 1)
    case "3": (-1, 2)
    case "4": (-2, 0)
    case "5": (-2, 1)
    case "6": (-2, 2)
    case "7": (-3, 0)
    case "8": (-3, 1)
    case "9": (-3, 2)
    default: fatalError("padPos")
    }
}

// from and to are on the dir pad, not num pad. Need separate function for that.
func getDirs(from: UInt8, to: UInt8) -> [[UInt8]] {
    if from == to {
        return [[]]
    }
    let (rfrom, cfrom) = padPos(from)
    let (rto, cto) = padPos(to)
    let dc = cto - cfrom
    let dr = rto - rfrom
    var first: [UInt8] = []
    if dr != 0 {
        let s = if dr < 0 { "^" } else { "v" }
        first.append(contentsOf: [UInt8](repeating: byteOf(s), count: abs(dr)))
    }
    if dc != 0 {
        let s = if dc < 0 { "<" } else { ">" }
        first.append(contentsOf: [UInt8](repeating: byteOf(s), count: abs(dc)))
    }
    let isDir = allDirs.contains(from) || allDirs.contains(to)
    let isNum = allNums.contains(from) || allNums.contains(to)
    if cto == 0 && dr == 1 && isDir{
        return [first]  // vertical first
    } else if isNum && rfrom == 0 && cto == 0 {
        return [first]
    }
    let rev = [UInt8](first.reversed())
    if cfrom == 0 && dr == -1 && isDir {
        return [rev]  // horizontal first
    } else if isNum && cfrom == 0 && rto == 0 {
        return [rev]
    } else if dr != 0 && dc != 0 {
        return [first, rev]
    } else {
        return [first]
    }
}

func getTrivialPrices() -> [String: Int] {
    var prices: [String: Int] = [:]
    for from in allDirs {
        for to in allDirs {
            // because it's the cost of starting at from, moving to to, and spitting it out.
            // spitting it out means just literally spitting out one to
            prices[toString(from, to)] = 1
        }
    }
    return prices
}

func totalPrice(_ bs: [UInt8], _ prev: [String: Int]) -> Int {
    var x = byteOf("A")
    var total = 0
    for b in bs {
        total += prev[toString(x, b)]!
        x = b
    }
    return total
}

func getNextPrices(_ prev: [String: Int], _ allChs: [UInt8]) -> [String: Int] {
    var cur: [String: Int] = [:]
    for from in allChs {
        for to in allChs {
            var best = Int.max
            for bs in getDirs(from: from, to: to) {
                let pr = totalPrice(bs + [byteOf("A")], prev)
                best = min(best, pr)
            }
            cur[toString(from, to)] = best
        }
    }
    return cur
}

func foo() {
    var prices = getTrivialPrices()
    for _ in 1...25 {  // 1...2 or 1...25
        prices = getNextPrices(prices, allDirs)
    }
    prices = getNextPrices(prices, allNums)
    let lines = ["789A", "968A", "286A", "349A", "170A"]
    for x in lines {
        print(totalPrice(x.asciiValues, prices))
    }
//    print(66 * 789 + 70 * 968 + 68 * 286 + 72 * 349 + 72 * 170)
    print(80786362260 * 789 + 86475783010 * 968 + 86475783008 * 286 + 87793663956 * 349 + 87513499936 * 170)
    // part 2
    // 217698355426872
}

foo()
