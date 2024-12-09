import Foundation

let filename = "/Users/xdavidliu/input09.txt"
let zero = Character("0").asciiValue!
let diskMap = getGrid(filename)[0].map{$0 - zero}
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

print("part 1 =", acc)  // 6258319840548
