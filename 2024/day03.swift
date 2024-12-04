let doRe = /do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\)/
func computeDoTotal(_ text: String, ignoreDo: Bool) -> Int {
    var total = 0
    var effect = true
    for m in text.matches(of: doRe) {
        if m.0 == "do()" {
            effect = true
        } else if m.0 == "don't()" {
            effect = false
        } else if effect || ignoreDo {
            total += Int(m.1!)! * Int(m.2!)!
        }
    }
    return total
}

let text = getLines("/Users/xdavidliu/input03.txt").joined()
// join because effect crosses over across lines
let p1 = computeDoTotal(text, ignoreDo: true)
let p2 = computeDoTotal(text, ignoreDo: false)
print("part 1 = \(p1)")  // 183380722
print("part 2 = \(p2)")  // 82733683
