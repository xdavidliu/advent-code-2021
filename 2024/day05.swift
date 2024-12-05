import Foundation

let lines = getLines("/Users/xdavidliu/input05.txt")

var i = 0
var pairs: [(Int, Int)] = []
var high = Int.min
while !lines[i].isEmpty {
    let a = lines[i].split(separator: "|")
    let x = Int(a[0])!
    let y = Int(a[1])!
    high = max(high, max(x, y))
    pairs.append((x, y))
    i += 1
}

var before = [[Bool]](repeating: [Bool](repeating: false, count: high+1), count: high+1)
for (x, y) in pairs {
    before[x][y] = true
}

i += 1  // skip empty line
var total = 0
var totalFixed = 0
while i < lines.count {
    var nums = lines[i].split(separator: ",").map{ Int($0)! }
    if (1..<nums.count).allSatisfy({ before[nums[$0-1]][nums[$0]] }) {
        total += nums[nums.count/2]
    } else {
        nums.sort{ before[$0][$1] }
        totalFixed += nums[nums.count/2]
    }
    i += 1
}

print("part 1 =", total)  // 4578
print("part 2 =", totalFixed)  // 6179
