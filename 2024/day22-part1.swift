import Foundation

let prune = 16777216

func iter(_ x: Int) -> Int {
    var y = ((x << 6) ^ x) % prune
    y = ((y >> 5) ^ y) % prune
    return ((y << 11) ^ y) % prune
}

func iter2000(_ y: Int) -> Int {
    var x = y
    for _ in 1...2000 {
        x = iter(x)
    }
    return x
}

func foo() {
    let filename = "/Users/xdavidliu/input22.txt"
    let nums = getLines(filename, omittingEmptySubsequences: true).map{Int($0)!}
    let finals = nums.map{iter2000($0)}.reduce(0, +)
    print("part 1 =", finals)  // 17163502021
}

foo()
