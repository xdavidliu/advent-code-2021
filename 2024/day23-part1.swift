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

func readProblem(_ filename: String) -> [String: Set<String>] {
    let lines = getLines(filename, omittingEmptySubsequences: true)
    var adj: [String: Set<String>] = [:]
    for line in lines {
        let (a, b) = separate(line)
        adj[a, default: []].insert(b)
        adj[b, default: []].insert(a)
    }
    return adj
}

func startT<T>(_ s: T) -> Bool where T: StringProtocol {
    return s.starts(with: "t")
}

func part1(_ adj: [String: Set<String>]) {
    var triple = Set<String>()
    for (a, bs) in adj {
        for b in bs {
            if b < a {
                continue
            }
            let alreadyT = startT(a) || startT(b)
            for node in adj.keys {
                if !alreadyT && !startT(node) {
                    continue
                }
                if node == a || node == b {
                    continue
                }
                if adj[a]!.contains(node) && adj[b]!.contains(node) {
                    triple.insert(canonical(a, b, node))
                }
            }
        }
    }
    print("part 1 =", triple.count)
}

// inefficient brute force, but fine because each row of adj is O(10)
func largest(_ a: String, _ adj: [String: Set<String>]) -> [String] {
    let bs = [String](adj[a]!)
    var out: [String] = []
    for i in 0..<bs.count-1 {
        if out.count > bs.count - i {
            break
        }
        var comp = [bs[i]]
        for k in i+1..<bs.count {
            if comp.allSatisfy({adj[bs[k]]!.contains($0)}) {
                comp.append(bs[k])
            }
        }
        if comp.count > out.count {
            out = comp
        }
    }
    out.append(a)
    out.sort()
    return out
}

func part2(_ adj: [String: Set<String>]) {
    var best: [String] = []
    for a in adj.keys {
        let cur = largest(a, adj)
        if cur.count > best.count {
            best = cur
        }
    }
    let p2 = best.joined(separator: ",")
    print("part 2 =", p2)
}

func solve() {
    let filename = "/Users/xdavidliu/input23.txt"
    let adj = readProblem(filename)
    part1(adj)  // 1154
    part2(adj)  // aj,ds,gg,id,im,jx,kq,nj,ql,qr,ua,yh,zn
}

solve()
