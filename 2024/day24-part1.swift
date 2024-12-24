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

func toString(_ op: Oper) -> String {
    return switch op {
    case .and:
        "AND"
    case .or:
        "OR"
    case .xor:
        "XOR"
    }
}

func ordered(_ a: String, _ b: String) -> (String, String) {
    return if a <= b {
        (a, b)
    } else {
        (b, a)
    }
}

func getRev(_ connect: Connects) -> [String: String] {
    var rev: [String: String] = [:]
    for (k, (a, op, b)) in connect {
        rev[canon(a, op, b)] = k
    }
    return rev
}

func getConnections<T>(_ block: T) -> Connects where T: StringProtocol {
    var out: Connects = [:]
    for line in block.split(separator: "\n") {
        let ws = line.split(separator: " ").map{String($0)}  // x00 AND y00 -> z00
        let (a, b) = ordered(ws[0], ws[2])
        out[ws[4]] = (a, fromString(ws[1]), b)
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

func padded(_ i: Int) -> String {
    return if i < 10 {
        "0" + String(i)
    } else {
        String(i)
    }
}

func padded(_ s: String, _ i: Int) -> String {
    return s + padded(i)
}

func canon(_ a: String, _ op: Oper, _ b: String) -> String {
    let (x, y) = ordered(a, b)
    return x + " " + toString(op) + " " + y
}

func debug(_ connect: inout Connects) {
    func rc(_ a: String, _ op: Oper, _ b: String) -> String {
        let x = rev[canon(a, op, b)]
        assert(x != nil)
        return x!
    }
    func swap(_ a: String, _ b: String) {
        (connect[a], connect[b]) = (connect[b]!, connect[a]!)
    }
    swap("qnw", "z15")
    swap("cqr", "z20")
    swap("nfj", "ncd")
    swap("vkg", "z37")
    let rev = getRev(connect)
    assert(rc("x00", .xor, "y00") == "z00")
    var carry = rc("x00", .and, "y00")
    var i = 1
    while true {
        let xi = padded("x", i)
        let yi = padded("y", i)
        let zi = padded("z", i)
        let oi = rc(xi, .xor, yi)
        let ai = rc(xi, .and, yi)
        let bi = rc(carry, .and, oi)
        // third failure here inside bi. a = trt, b = ncd
        // i = 27
        // x27 XOR y27 -> ncd is in input. It might be wrong. Look for
        // rule like "trt AND" or "AND trt", and if found, swap the other
        // one with ncd. Yep found:
        // nfj AND trt -> hrn
        let actual = rc(carry, .xor, oi)
        assert(actual == zi, actual)
        // first failure here. Gives these connects:
        // ctg XOR rjm -> qnw
        // dnn OR mrm -> z15
        // second failure here. Gives these connects:
        // wrb XOR msn -> cqr
        // x20 AND y20 -> z20
        // fourth failure here. Gives these connects:
        // fcm XOR dnt -> vkg
        // dnt AND fcm -> z37
        carry = rc(ai, .or, bi)
        if carry.starts(with: "z") {
            return  // very last z is treated as a carry
        }
        i += 1
    }
}

func solve() {
    let filename = "/Users/xdavidliu/input24.txt"
    var (wire, connect) = readProblem(filename)
    let bz = bitsOfPrefix("z", &wire, connect)
    print("part 1 =", sumBits(bz))  // 48508229772400
    debug(&connect)  // does nothing. Swapped below taken from manual experimentation inside debugged
    var swapped = ["qnw", "z15", "cqr", "z20", "nfj", "ncd", "vkg", "z37"]
    swapped.sort()
    print("part 2 =", swapped.joined(separator: ","))
    // cqr,ncd,nfj,qnw,vkg,z15,z20,z37
}

solve()
