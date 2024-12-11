import Foundation

let sample0: [Int64] = [0, 1, 10, 99, 999]
let sample1: [Int64] = [125, 17]
let count1 = 6

let input11: [Int64] = [7568, 155731, 0, 972, 1, 6919238, 80646, 22]
let count11 = 25

func digitCount(_ num: Int64) -> Int {
    var c = 0
    var x = num
    while x != 0 {
        x /= 10
        c += 1
    }
    return c
}

func pow10(_ expt: Int) -> Int64 {
    var out: Int64 = 1
    for _ in 0..<expt {
        out *= 10
    }
    return out
}

func splitEvenDigits(_ num: Int64) -> Optional<(Int64, Int64)> {
    let dc = digitCount(num)
    if !dc.isMultiple(of: 2) {
        return nil
    }
    let p10 = pow10(dc / 2)
    return (num / p10, num % p10)
}

func evolve(_ num: Int64) -> [Int64] {
    if num == 0 {
        return [1]
    }
    let split = splitEvenDigits(num)
    if split == nil {
        let prod = num * 2024
        if prod < num {
            fatalError("overflow detected")
        }
        return [prod]
    } else {
        return [split!.0, split!.1]
    }
}

func blinkOnce(_ arr: [Int64]) -> [Int64] {
    return arr.flatMap{ evolve($0) }
}

var a = input11
for _ in 0..<count11 {
    a = blinkOnce(a)
}
print("part 1 =", a.count)  // 186424
