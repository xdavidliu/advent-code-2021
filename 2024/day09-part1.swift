import Foundation

let filename = "/Users/xdavidliu/sample.txt"
let diskMap = readDiskMap(filename)
let fileData = convertToFileData(diskMap)
print("part 1 =", part1(fileData))  // 6258319840548
let table = makeTable(diskMap)

func makeTable(_ diskMap: [UInt8]) -> [[Int]] {
    var out = [[Int]](repeating: [], count: 10)
    for i in stride(from: 0, to: diskMap.count, by: 2) {
        out[Int(diskMap[i])].append(i)
    }
    return out
}

func readDiskMap(_ filename: String) -> [UInt8] {
    return getGrid(filename)[0].map{$0 - Character("0").asciiValue!}
}

func convertToFileData(_ diskMap: [UInt8]) -> [Int] {
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
