import Foundation

let filename = "/Users/xdavidliu/sample.txt"
let diskMap = readDiskMap(filename)
let fileData = convertToFileData(diskMap)
print("part 1 =", part1(fileData))  // 6258319840548
print("part 2 =", part2(diskMap))  //
// 10690294388280 too high
// 2318
// expect 2858

func sumFormula(_ n: Int) -> Int {
    return n * (n+1) / 2
}

func popRightmostAtMost(_ length: Int, _ table: inout [[Int]]) -> Optional<Int> {
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

func printVal(val: Int, n: Int) {
    for _ in 0..<n {
        print(val, terminator: "")
    }
}

func printDots(n: Int) {
    for _ in 0..<n {
        print(".", terminator: "")
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
//                printVal(val: addr, n: length)
                total += addr * (sumFormula(dataCount - 1 + length) - sumFormula(dataCount - 1))
            } else {
//                printDots(n: length)
            }
            dataCount += length
        } else {
            var freeRemain = length
            while freeRemain > 0 {
                if let right = popRightmostAtMost(freeRemain, &table) {
                    let rightLength = diskMap[right]
                    disabled[right] = true
                    let addr = right / 2
//                    printVal(val: addr, n: rightLength)
                    total += addr * (sumFormula(dataCount - 1 + rightLength) - sumFormula(dataCount - 1))
                    dataCount += rightLength
                    freeRemain -= rightLength
                } else {
                    break
                }
            }
            if freeRemain > 0 {
//                printDots(n: freeRemain)
            }
            dataCount += freeRemain
        }
        left += 1
    }
//    print()
    return total
}

/*
 ah, address can be obtained by index in diskMap. That's done BEFORE the moving
 from ind, what's addr
 ind is even number. 0, 2, 4. Ah, it's just addr = ind / 2
 
 sum from x to x + y
 2 + 3 + 4 + 5
 = sum to 5 - sum to (2-1)
 = 5(6)/2 - 1(2)/2
 = 15 - 1 = 14
 
 (x+y)(x+y+1)/2 - x(x-1)/2
 
 length is the value, so track how many seen so far.
 
 */

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


/*

 have list with key being from 1-9 and value being list of indices
 go thru input and for each data (not free) so odd index with length x, append to
 the x-th slot in the top list
 
 now go thru each free. Suppose length y. Then want rightmost index of z <= y.
 Note this rightmost index must be > current left.
 search thru list for that, in O(1) time. pop that one, now free is y - z. Repeat
 until the free is 0 or there does not exist ANY z satisfying this.
 
 Then move on to next free, and repeat.
 
 while this is being done, accumulate score. Keep track of which right ones are moved,
 that way you can omit them when summing.
 
 
 */
