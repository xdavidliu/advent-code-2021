func getTwoArrays(lines: [String]) -> ([Int], [Int]) {
    var a: [Int] = [], b: [Int] = []
    for line in lines {
        let toks = line.split(separator: " ")
        a.append(Int(toks[0])!)
        b.append(Int(toks[1])!)
    }
    a.sort()
    b.sort()
    return (a, b)
}

func getCounter(nums: [Int]) -> [Int: Int] {
    var d: [Int: Int] = [:]
    for x in nums {
        d[x, default: 0] += 1
    }
    return d
}

// hack: if put in Documents folder, it will keep asking permission.
// astonishingly, home dir does not have this security
let lines = getLines(fileName: "/Users/xdavidliu/input01.txt")
let (a, b) = getTwoArrays(lines: lines)
var sum = 0
for (x, y) in zip(a, b) {
    sum += abs(x - y)
}
print("part 1 = \(sum)")
// 1970720

var p2 = 0
let bCount = getCounter(nums: b)
for x in a {
    p2 += x * bCount[x, default: 0]
}
print("part 2 = \(p2)")
// 17191599
