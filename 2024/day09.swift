import Foundation

let filename = "/Users/xdavidliu/input09.txt"
let diskMap = readDiskMap(filename)
let fileData = convertToFileData(diskMap)
print("part 1 =", part1(fileData))  // 6258319840548
print("part 2 =", part2(diskMap))  // 6286182965311

func sumFormula(_ n: Int) -> Int {
    return n * (n+1) / 2
}

func popRightmostAtMost(_ length: Int, _ table: inout [[Int]])
-> Optional<Int> {
    var rightMost = Int.min
    var rightMostInd: Optional<Int> = nil
    for i in 1...length {
        if let x = table[i].last {
            if x > rightMost {
                rightMost = x
                rightMostInd = i
            }
        }
    }
    if let i = rightMostInd {
        return table[i].popLast()
    } else {
        return nil
    }
}

func part2(_ diskMap: [Int]) -> Int {
    var left = 0
    var total = 0
    var dataCount = 0
    var disabled = [Bool](repeating: false, count: diskMap.count)
    var table = makeTable(diskMap)
    while left < diskMap.count {
        let length = Int(diskMap[left])
        if left.isMultiple(of: 2) {
            if !disabled[left] {
                let addr = left / 2
                total += addr * (sumFormula(dataCount - 1 + length) - sumFormula(dataCount - 1))
                disabled[left] = true
            }
            dataCount += length
        } else {
            var freeRemain = length
            while freeRemain > 0 {
                if let right = popRightmostAtMost(freeRemain, &table) {
                    if right < left {
                        break
                    }
                    let rightLength = diskMap[right]
                    disabled[right] = true
                    let addr = right / 2
                    total += addr * (sumFormula(dataCount - 1 + rightLength) - sumFormula(dataCount - 1))
                    dataCount += rightLength
                    freeRemain -= rightLength
                } else {
                    break
                }
            }
            dataCount += freeRemain
        }
        left += 1
    }
    return total
}

func makeTable(_ diskMap: [Int]) -> [[Int]] {
    var out = [[Int]](repeating: [], count: 10)
    for i in stride(from: 0, to: diskMap.count, by: 2) {
        out[Int(diskMap[i])].append(i)
    }
    return out
}

func readDiskMap(_ filename: String) -> [Int] {
    return getGrid(filename)[0].map{Int($0 - Character("0").asciiValue!)}
}

func convertToFileData(_ diskMap: [Int]) -> [Int] {
    var fileData: [Int] = []
    var isFree = false
    var addr = 0
    let freeSpace = -1
    for count in diskMap {
        let ch = if isFree { freeSpace } else { addr }
        for _ in 0..<count {
            fileData.append(ch)
        }
        if isFree {
            addr += 1
        }
        isFree = !isFree
    }
    return fileData
}

func part1(_ fileData: [Int]) -> Int {
    var left = 0
    var right = fileData.count
    var acc = 0

    while left < right {
        let id = fileData[left]
        if id != -1 {
            acc += id * left
        } else {
            right -= 1
            while left < right && fileData[right] == -1 {
                right -= 1
            }
            if left == right {
                break
            } else {
                acc += left * fileData[right]
            }
        }
        left += 1
    }
    return acc
}
