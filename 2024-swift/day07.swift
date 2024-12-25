import Foundation

func works(_ ans: Int64, _ ops: [Int], part2: Bool) -> Bool {
    return worksRec(acc: Int64(ops[0]), pos: 1, ans: ans, ops, part2: part2)
}

func concatNumber(_ a: Int64, _ b: Int64) -> Int64 {
    return Int64(String(a) + String(b))!
}

func worksRec(acc: Int64, pos: Int, ans: Int64, _ ops: [Int], part2: Bool) -> Bool {
    if pos == ops.count {
        return ans == acc
    } else {
        let op = Int64(ops[pos])
        return worksRec(acc: acc+op, pos: pos+1, ans: ans, ops, part2: part2) ||
        worksRec(acc: acc*op, pos: pos+1, ans: ans, ops, part2: part2) ||
        part2 && worksRec(acc: concatNumber(acc, op), pos: pos+1, ans: ans, ops, part2: part2)
    }
}

func solve(_ lines: [String], part2: Bool = false) -> Int64 {
    
    var total: Int64 = 0
    for line in lines {
        let parts = line.split(separator: ": ")
        let ans = Int64(parts[0])!
        let ops = parts[1].split(separator: " ").map{Int($0)!}
        if works(ans, ops, part2: part2) {
            total += ans
        }
    }
    return total
}

let lines = getLines("/Users/xdavidliu/input07.txt", omittingEmptySubsequences: true)
let p1 = solve(lines)
print("part 1 = \(p1)")  // 303766880536
let p2 = solve(lines, part2: true)
print("part 2 = \(p2)")  // 337041851384440
