import Foundation

// note: if any days fail with cryptic errors, prob because of wrong value of
// omittingEmptySubsequences, combined with last line being empty.
func getLines(_ fileName: String, omittingEmptySubsequences: Bool = false) -> [String] {
    do {
        let url = URL(fileURLWithPath: fileName)
        let text = try String(contentsOf: url, encoding: .utf8)
        return text.split(separator: "\n", omittingEmptySubsequences: omittingEmptySubsequences).map(String.init)
    } catch {
        return []
    }
}

func getGrid(_ filename: String) -> [[UInt8]] {
    return getLines(filename, omittingEmptySubsequences: true)
        .map{$0.asciiValues}
}

// https://stackoverflow.com/a/29835826/2990344

extension StringProtocol {
    var asciiValues: [UInt8] { compactMap(\.asciiValue) }
}

//https://docs.swift.org/swift-book/documentation/the-swift-programming-language/stringsandcharacters/#String-Indices
// "Swift strings can’t be indexed by integer values."

func singleInd(r: Int, c: Int, nc: Int) -> Int {
    return c + r * nc
}

func isValidIndex<T>(_ grid: [[T]], r: Int, c: Int) -> Bool {
    return grid.indices.contains(r) && grid[r].indices.contains(c)
}

func splitInd(i: Int, nc: Int) -> (Int, Int) {
    let r = i / nc
    let c = i % nc
    return (r, c)
}

func gcd(_ a: Int, _ b: Int) -> Int {
    if a < b {
        return gcd(b, a)
    } else if b == 0 {
        return a
    } else {
        return gcd(b, a % b)
    }
}

// https://docs.swift.org/swift-book/documentation/the-swift-programming-language/generics/
struct Queue<Element> {
    var front: [Element] = []
    var back: [Element] = []
    var isEmpty: Bool {
        get {
            return front.isEmpty && back.isEmpty
        }
    }
    mutating func add(_ item: Element) {
        back.append(item)
    }
    mutating func remove() -> Element {
        if front.isEmpty {
            while back.count > 1 {
                front.append(back.popLast()!)
            }
            return back.popLast()!
        } else {
            return front.popLast()!
        }
    }
}
