import Foundation

// todo: use makeGrid in 10 5 14 16 9 15, all days that manually create
// repeating grid.

// todo: replace all Character instances for brevity
func byteOf(_ s: String) -> UInt8 {
    return Character(s).asciiValue!
}

// note: if any days fail with cryptic errors, prob because of wrong value of
// omittingEmptySubsequences, combined with last line being empty.
func getLines(_ fileName: String, omittingEmptySubsequences: Bool = false) -> [String] {
    let text = getText(fileName)
    return text.split(separator: "\n", omittingEmptySubsequences: omittingEmptySubsequences).map(String.init)
}

func getText(_ fileName: String) -> String {
    do {
        let url = URL(fileURLWithPath: fileName)
        return try String(contentsOf: url, encoding: .utf8)
    } catch {
        return ""
    }
}

func makeGrid<T>(elem: T, nr: Int, nc: Int) -> [[T]] {
    return [[T]](repeating: [T](repeating: elem, count: nc), count: nr)
}

func getGrid(_ filename: String) -> [[UInt8]] {
    return getLines(filename, omittingEmptySubsequences: true)
        .map{$0.asciiValues}
}

func plot(_ grid: [[UInt8]]) {
    for row in grid {
        print(String(bytes: row, encoding: .utf8)!)
    }
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

func singleIndWithD(r: Int, c: Int, id: Int, nc: Int, nr: Int) -> Int {
    return id * nr * nc + r * nc + c
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
    private var front: [Element] = []
    private var back: [Element] = []
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

// based on 2019 day 18 in common lisp, based on CLRS
struct MinHeap<Element> {
    private var arr: [(Int, Element)] = []
    
    var count: Int {
        get {
            return arr.count
        }
    }
    
    var isEmpty: Bool {
        get {
            return arr.isEmpty
        }
    }
    
    private static func parent (_ i: Int) -> Int {
        return (i-1) / 2
    }
    
    private static func leftChild(_ i: Int) -> Int {
        return 1 + 2 * i
    }
    
    private static func rightChild(_ i: Int) -> Int {
        return 2 + 2 * i
    }
    
    private func indMin(_ i: Int, _ k: Int) -> Int {
        return if arr[i].0 <= arr[k].0 { i } else { k }
    }
    
    private mutating func minHeapify(_ i: Int) {
        let l = MinHeap.leftChild(i)  // silly that this needs MinHeap prefix
        let r = MinHeap.rightChild(i)
        var m = i
        if l < count {
            m = indMin(m, l)
        }
        if r < count {
            m = indMin(m, r)
        }
        if m != i {
            (arr[m], arr[i]) = (arr[i], arr[m])
            minHeapify(m)
        }
    }
    
    mutating func pop() -> (Int, Element) {
        let last = count - 1
        (arr[0], arr[last]) = (arr[last], arr[0])
        let minElem = arr.popLast()!
        minHeapify(0)
        return minElem
    }

    private mutating func decreaseKey(_ k: Int, _ toVal: Int) {
        var i = k
        while i != 0 && toVal < arr[MinHeap.parent(i)].0 {
            (arr[i], arr[MinHeap.parent(i)]) = (arr[MinHeap.parent(i)], arr[i])
            i = MinHeap.parent(i)
        }
        if toVal < arr[i].0 {
            arr[i] = (toVal, arr[i].1)
        }
    }
    
    mutating func insert(_ key: Int, _ elem: Element) {
        arr.append((Int.max, elem))
        decreaseKey(arr.count - 1, key)
    }
}
