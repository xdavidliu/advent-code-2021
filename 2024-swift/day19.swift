import Foundation

func readProblem(_ filename: String) -> (Set<[UInt8]>, (Int, Int), [[UInt8]]) {
    let text = getText(filename)
    let blocks = text.split(separator: "\n\n")
    let towelArr = blocks[0].split(separator: ", ").map{$0.asciiValues}
    let designs = blocks[1].split(separator: "\n").map{$0.asciiValues}
    let towels = Set<[UInt8]>(towelArr)
    let smallN = towels.map{$0.count}.min()!
    let largeN = towels.map{$0.count}.max()!
    return (towels, (smallN, largeN), designs)
}

func solve() {
    let filename = "/Users/xdavidliu/input19.txt"
    let (towels, (smallN, largeN), designs) = readProblem(filename)
    var p1 = 0
    var p2 = 0
    for des in designs {
        let cm = countMatch(towels, des, small: smallN, large: largeN)
        if 0 != cm {
            p1 += 1
        }
        p2 += cm
    }
    print("part 1 =", p1)  // 298
    print("part 2 =", p2)  // 572248688842069
}

// resembles the DP array solution for the "ways to make change" problem
func countMatch(_ towels: Set<[UInt8]>, _ line: [UInt8], small: Int, large: Int) -> Int {
    var bs = [Int](repeating: 0, count: 1+line.count)
    bs[0] = 1
    for k in 1..<bs.count {
        for m in small...large {
            if k < m {
                break
            }
            if towels.contains(Array(line[k-m..<k])) {
                bs[k] += bs[k-m]
            }
        }
    }
    return bs.last!
}

solve()
