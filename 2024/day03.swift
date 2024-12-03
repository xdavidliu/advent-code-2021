// broken version that segfaults

import Foundation

func getLines(_ fileName: String) -> [String.SubSequence] {
    do {
        let url = URL(fileURLWithPath: fileName)
        let text = try String(contentsOf: url, encoding: .utf8)
        return text.split(separator: "\n")
    } catch {
        return []
    }
}

let lines = getLines("/Users/xdavidliu/input03.txt")
//let p1 = lines.map{ computeTotal($0, ignoreDo: true) }.reduce(0, +)
let p1 = lines.map{ simpleComputeTotal($0) }.reduce(0, +)
// let p2 = lines.map{ computeTotal($0, ignoreDo: false) }.reduce(0, +)
print("part 1 = \(p1)")  // 183380722
// print("part 2 = \(p2)")  // 183380722

let mulDoRe = /do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\)/

func computeTotal(_ text: String.SubSequence, ignoreDo: Bool) -> Int {
    var total = 0
    var effect = true
    for m in text.matches(of: mulDoRe) {
        if m.0 == "do()" {
            effect = true
        } else if m.0 == "don't()" {
            effect = false
        } else if ignoreDo || effect {
            if m.0.starts(with: "mul") {
                total += Int(m.1!)! * Int(m.2!)!
            }
        }
    }
    return total
}

let mulRe = /mul\((\d{1,3}),(\d{1,3})\)/
let re = /mul\(.+\)/

func simpleComputeTotal(_ text: String.SubSequence) -> Int {
    var total = 0
    for m in text.matches(of: re) {
        
//        total += Int(m.1)! * Int(m.2)!
    }
    return total
}
