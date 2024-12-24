import Foundation

func getInitialWires<T>(_ block: T) -> [String: Int] where T: StringProtocol {
    var out: [String: Int] = [:]
    for line in block.split(separator: "\n") {
        let ws = line.split(separator: ": ")  // x00: 1
        out[String(ws[0])] = Int(ws[1])!
    }
    return out
}

enum Oper {
    case and, xor, or
}

typealias Connects = [String: (String, Oper, String)]

func fromString(_ s: String) -> Oper {
    return switch s {
    case "AND":
        Oper.and
    case "OR":
        Oper.or
    case "XOR":
        Oper.xor
    default:
        fatalError("fromString")
    }
}

func getConnections<T>(_ block: T) -> Connects where T: StringProtocol {
    var out: Connects = [:]
    for line in block.split(separator: "\n") {
        let ws = line.split(separator: " ").map{String($0)}  // x00 AND y00 -> z00
        out[ws[4]] = (ws[0], fromString(ws[1]), ws[2])
    }
    return out
}

func readProblem(_ filename: String) -> ([String: Int], Connects) {
    let blocks = getText(filename).split(separator: "\n\n")
    let wire = getInitialWires(blocks[0])
    let connect = getConnections(blocks[1])
    return (wire, connect)
}

/*
 idea: have wires be table of values. Use top-down dp. For each connect, go thru and
 populate the value. At end, get all z values, sort by alpha, then spit out bits,
 compute integer.
 */

func operate(_ v1: Int, _ op: Oper, _ v2: Int) -> Int {
    return switch op {
    case .and:
        v1 & v2
    case .or:
        v1 | v2
    case .xor:
        v1 ^ v2
    }
}

func getValue(_ key: String, _ wire: inout [String: Int], _ connect: Connects) -> Int {
    if let already = wire[key] {
        return already
    }
    let (k1, op, k2) = connect[key]!
    let v1 = getValue(k1, &wire, connect)
    let v2 = getValue(k2, &wire, connect)
    let ans = operate(v1, op, v2)
    wire[key] = ans
    return ans
}

func sumBits(_ bs: [Int]) -> Int {
    var out = 0
    for b in bs.reversed() {
        out <<= 1
        out += b
    }
    return out
}

func allNames(_ pref: String, _ wire: [String: Int], _ connect: Connects) -> [String] {
    var out = Set<String>(wire.keys.filter{$0.starts(with: pref)})
    out.formUnion(connect.keys.filter({$0.starts(with: pref)}))
    var arr = [String](out)
    arr.sort()
    return arr
}

func bitsOfPrefix(_ pref: String, _ wire: inout [String: Int], _ connect: Connects) -> [Int] {
    let names = allNames(pref, wire, connect)
    let bs = names.map{getValue($0, &wire, connect)}
    return bs
}

func solve() {
    let filename = "/Users/xdavidliu/sample.txt"  // 24
    var (wire, connect) = readProblem(filename)
    let bz = bitsOfPrefix("z", &wire, connect)
    let bx = bitsOfPrefix("x", &wire, connect)
    let by = bitsOfPrefix("y", &wire, connect)
    print("part 1 =", sumBits(bz))  // 48508229772400
    
}

solve()

/*
 okay, can print expected value by doing sumbits one plus other
 
 okay, go thru and when find discrepency, just fix it for free and note down
 which one. Then go to next one, fix for free, and note down. Should find four I think.
 */
