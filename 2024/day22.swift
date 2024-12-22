import Foundation

let prune = 16777216 - 1
let pow19to3 = 19 * 19 * 19

func iter(_ x: Int) -> Int {
    var y = ((x << 6) ^ x) & prune
    y = (y >> 5) ^ y
    return ((y << 11) ^ y) & prune
}

func nextSlide(acc: Int, diff: Int) -> Int {
    return (acc % pow19to3) * 19 + diff + 9
}

func arr2slide(_ arr: [Int]) -> Int {
    var s = 0
    for x in arr {
        s = nextSlide(acc: s, diff: x)
    }
    return s
}

func iter2000(_ y: Int, _ outer: inout [Int: Int]) -> Int {
    var x = y
    var inner: [Int: Int] = [:]
    x = iter(x)
    var slide = 0
    var dig = x % 10
    func perform() {
        let dig0 = dig
        x = iter(x)
        dig = x % 10
        let diff = dig - dig0
        slide = nextSlide(acc: slide, diff: diff)
    }
    for _ in 2...4 {
        perform()
    }
    for _ in 5...2000 {
        perform()
        if inner[slide] == nil {
            inner[slide] = dig
        }
    }
    for (k, v) in inner {
        outer[k] = outer[k, default: 0] + v
    }
    return x
}

func solve() {
    let filename = "/Users/xdavidliu/input22.txt"
    let nums = getLines(filename, omittingEmptySubsequences: true).map{Int($0)!}
    var outer: [Int: Int] = [:]
    let p1 = nums.map{iter2000($0, &outer)}.reduce(0, +)
    print("part 1 =", p1)  // 17163502021
    let p2 = outer.values.max()!
    print("part 2 =", p2)  // 1938
}

solve()
