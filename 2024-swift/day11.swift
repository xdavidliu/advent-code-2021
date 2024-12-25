import Foundation

let input = [7568: 1, 155731: 1, 0: 1, 972: 1, 1: 1, 6919238: 1, 80646: 1, 22: 1]

var a = input
for i in 0..<75 {
    if i == 25 {
        print("part 1 =", totalSize(a))  // 186424
    }
    a = blinkOnce(a)
}
print("part 2 =", totalSize(a))  // 219838428124832

func totalSize(_ counter: Dictionary<Int, Int>) -> Int {
    var total = 0
    for (_, times) in counter {
        total += times
    }
    return total
}

func digitCount(_ num: Int) -> Int {
    var c = 0
    var x = num
    while x != 0 {
        x /= 10
        c += 1
    }
    return c
}

func pow10(_ expt: Int) -> Int {
    var out: Int = 1
    for _ in 0..<expt {
        out *= 10
    }
    return out
}

func splitEvenDigits(_ num: Int) -> Optional<(Int, Int)> {
    let dc = digitCount(num)
    if !dc.isMultiple(of: 2) {
        return nil
    }
    let p10 = pow10(dc / 2)
    return (num / p10, num % p10)
}

func evolve(value: Int, times: Int, _ counter: inout Dictionary<Int, Int>) {
    if value == 0 {
        counter[1, default: 0] += times
        return
    }
    let split = splitEvenDigits(value)
    if split == nil {
        let prod = value * 2024
        if prod < value {
            fatalError("overflow detected")
        }
        counter[prod, default: 0] += times
    } else {
        counter[split!.0, default: 0] += times
        counter[split!.1, default: 0] += times
    }
}

func blinkOnce(_ counter: Dictionary<Int, Int>) -> Dictionary<Int, Int> {
    var out = Dictionary<Int, Int>()
    for (value, times) in counter {
        evolve(value: value, times: times, &out)
    }
    return out
}
