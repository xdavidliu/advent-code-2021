func eitherUpOrDown(_ nums: [Int], grace: Bool) -> Bool {
    return allBetweenOneThree(nums, sign: 1, start: 1, grace: grace)
    || allBetweenOneThree(nums, sign: -1, start: 1, grace: grace)
}

func parseNums(_ line: String) -> [Int] {
    return line.split(separator: " ").map { Int($0)! }
}

let lines = getLines(fileName: "/Users/xdavidliu/input02.txt")

let p1 = lines.filter { eitherUpOrDown(parseNums($0), grace: false) }.count
print("part 1 = \(p1)")  // 287

let p2 = lines.filter { eitherUpOrDown(parseNums($0), grace: true) }.count
print("part 2 = \(p2)")  // 354

func betweenOneThree(_ x: Int) -> Bool {
    return 1 <= x && x <= 3
}

func allBetweenOneThree(_ nums: [Int], sign: Int, start: Int, grace: Bool) -> Bool {
    if start >= nums.count {
        return true
    }
    for i in start..<nums.count {
        if !betweenOneThree(sign * (nums[i] - nums[i-1])) {
            if !grace {
                return false
            }
            let canRemoveLeft = i == 1 || betweenOneThree(sign * (nums[i] - nums[i-2]))
            let canRemoveRight = i+1 == nums.count || betweenOneThree(sign * (nums[i+1] - nums[i-1]))
            return canRemoveLeft && allBetweenOneThree(nums, sign: sign, start: i+1, grace: false)
            || canRemoveRight && allBetweenOneThree(nums, sign: sign, start: i+2, grace: false)
        }
    }
    return true
}
