lines = readlines("/home/xdavidliu/Documents/aoc/input10.txt")
nums = [parse(Int, x) for x in lines]
push!(nums, 0)
push!(nums, 3+maximum(nums))
sort!(nums)
n = length(nums)
cs = [0, 0, 0]
for i in 1:n-1
    d = nums[i+1]-nums[i]
    cs[d] += 1
end
println("part 1 = ", cs[1] * cs[3])  # 2201
dp = repeat([0], n+2)
dp[n] = 1
for i in reverse(1:n)
    k = i-1
    while 0 < k && nums[k] >= nums[i] - 3
        dp[k] += dp[i]
        k -= 1
    end
end
println("part 2 = ", dp[1])  # 169255295254528
