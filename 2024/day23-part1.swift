import Foundation

// https://stackoverflow.com/a/47434831/2990344
func canonical<T>(_ a: T, _ b: T) -> String where T: StringProtocol {
    return if a < b {
        a + "-" + b
    } else {
        b + "-" + a
    }
}

func canonical<T>(_ a: T, _ b: T, _ c: T) -> String where T: StringProtocol {
    var xs = [a, b, c]
    xs.sort()
    return xs[0] + "-" + xs[1] + "-" + xs[2]
}

func separate(_ word: String) -> (String, String) {
    let ws = word.split(separator: "-")
    return (String(ws[0]), String(ws[1]))
}

func readProblem(_ filename: String) -> (Set<String>, Set<String>) {
    let lines = getLines(filename, omittingEmptySubsequences: true)
    var connects = Set<String>()
    var nodes = Set<String>()
    for line in lines {
        let (a, b) = separate(line)
        connects.insert(canonical(a, b))
        nodes.insert(a)
        nodes.insert(b)
    }
    return (nodes, connects)
}

func startT<T>(_ s: T) -> Bool where T: StringProtocol {
    return s.starts(with: "t")
}

func solve() {
    let filename = "/Users/xdavidliu/input23.txt"
    let (nodes, connects) = readProblem(filename)
    var triple = Set<String>()
    for line in connects {
        let (a, b) = separate(line)
        let alreadyT = startT(a) || startT(b)
        for node in nodes {
            if !alreadyT && !startT(node) {
                continue
            }
            if node == a || node == b {
                continue
            }
            if connects.contains(canonical(a, node)) && connects.contains(canonical(b, node)) {
                triple.insert(canonical(a, b, node))
            }
        }
    }
    print("part 1 =", triple.count)  // 1154
}

solve()
