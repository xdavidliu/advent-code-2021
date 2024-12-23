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
    print("part 1 =", triple.count)  // 1154
}

func bfs(_ a: String, _ adj: [String: Set<String>], _ seen: inout Set<String>, _ comps: inout [Set<String>]) {
    if seen.contains(a) {
        return
    }
    var comp = Set<String>()
    seen.insert(a)
    comp.insert(a)
    var que = Queue<String>()
    que.add(a)
    while !que.isEmpty {
        let front = que.remove()
        for nb in adj[front]! {
            if seen.contains(nb) {
                continue
            }
            seen.insert(nb)
            comp.insert(nb)
            que.add(nb)
        }
    }
    comps.append(comp)
}

func part2(_ adj: [String: Set<String>]) {
    var comps: [Set<String>] = []
    var seen: Set<String> = []
    for a in adj.keys {
        bfs(a, adj, &seen, &comps)
    }
    for comp in comps {
        print(comp.count)
    }
}

func solve() {
    let filename = "/Users/xdavidliu/sample.txt"
    let adj = readProblem(filename)
    part1(adj)
    part2(adj)
}

solve()


/*
 seen is set of seen
 go thru adj and start bfs from each
 store in component, and also in seen
 if it's in seen, don't go to it
 after done, add component to out. Out is list of sets
 return out when done
 */
