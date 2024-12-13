import Foundation

func sameFraction(_ a: (Int, Int), _ b: (Int, Int)) -> Bool {
    let ga = gcd(a.0, a.1)
    let gb = gcd(b.0, b.1)
    return a.0 / ga == b.0 / gb && a.1 / ga == b.1 / gb
}

func solve(xa: Int, ya: Int, xb: Int, yb: Int, px: Int, py: Int) -> Optional<(Int, Int)> {
    let det = xa * yb - xb * ya
    assert(det != 0)  // seems to be true for my data
    // if det == 0 and (px, py) were reachable, then I'm not sure how to answer because
    // it's either all a, all b, or some mixture of them just to get to integer px, py
    let aa = yb * px - xb * py
    let bb = xa * py - ya * px
    if !(aa % det == 0 && bb % det == 0) {
        // there exists a solution but not in whole numbers
        return nil
    } else {
        return (aa / det, bb / det)
    }
}

func bothParts() {
    let filename = "/Users/xdavidliu/input13.txt"
    let text = getText(filename)
    let blocks = text.split(separator: "\n\n").map(String.init)
    var toks1 = 0
    var toks2 = 0
    for blk in blocks {
        let (p1, p2) = parseAndSolve(blk)
        if let (a, b) = p1 {
            toks1 += 3 * a + b
        }
        if let (a, b) = p2 {
            toks2 += 3 * a + b
        }
    }
    print("part 1 =", toks1)  // 29711
    print("part 2 =", toks2)  // 94955433618919
}

func parseAndSolve(_ block: String) -> (Optional<(Int, Int)>, Optional<(Int, Int)>) {
    let lines = block.split(separator: "\n")
    assert(lines.count == 3)
    let regButton = /Button \w: X\+(\d+), Y\+(\d+)/
    let regPrize = /Prize: X=(\d+), Y=(\d+)/
    let mA = lines[0].wholeMatch(of: regButton)!
    let mB = lines[1].wholeMatch(of: regButton)!
    let mP = lines[2].wholeMatch(of: regPrize)!
    let (xa, ya) = (Int(mA.1)!, Int(mA.2)!)
    let (xb, yb) = (Int(mB.1)!, Int(mB.2)!)
    let (px, py) = (Int(mP.1)!, Int(mP.2)!)
    let shift = 10_000_000_000_000
    let p1 = solve(xa: xa, ya: ya, xb: xb, yb: yb, px: px, py: py)
    let p2 = solve(xa: xa, ya: ya, xb: xb, yb: yb, px: px + shift, py: py + shift)
    return (p1, p2)
}

bothParts()
