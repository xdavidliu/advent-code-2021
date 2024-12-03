import Foundation

let re = /mul\((\d{1,3}),(\d{1,3})\)/

func computeTotal(_ text: String) -> Int {
    var total = 0
    for m in text.matches(of: re) {
        total += Int(m.1)! * Int(m.2)!
    }
    return total
}

let doRe = /do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\)/
func computeDoTotal(_ text: String) -> Int {
    var total = 0
    var effect = true
    for m in text.matches(of: doRe) {
        if m.0 == "do()" {
            effect = true
        } else if m.0 == "don't()" {
            effect = false
        } else if effect {
            if m.0.starts(with: "mul") {
                total += Int(m.1!)! * Int(m.2!)!
            }
        }
    }
    return total
}

func getLines(_ fileName: String) -> [String] {
    do {
        let url = URL(fileURLWithPath: fileName)
        let text = try String(contentsOf: url, encoding: .utf8)
        return text.split(separator: "\n").map(String.init)
    } catch {
        return []
    }
}

let text = getLines("/Users/xdavidliu/input03.txt").joined()
// join because effect crosses over across lines
let p1 = computeTotal(text)
let p2 = computeDoTotal(text)
print("part 1 = \(p1)")  // 183380722
print("part 2 = \(p2)")  // 82733683
