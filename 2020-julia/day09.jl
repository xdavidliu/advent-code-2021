readnums(fname) = [parse(Int, x) for x in readlines(fname)]

function preamblecounts(nums, npre)
    ct = Dict{Int, Int}()
    for i in 1:npre-1
        for k in i+1:npre
            key = nums[i] + nums[k]
            ct[key] = get(ct, key, 0) + 1
        end
    end
    ct
end

function decr!(cts, key)
    c = get(cts, key, -1)
    @assert c != -1
    if c == 1
        delete!(cts, key)
    else
        cts[key] = c - 1
    end
end

function incr!(cts, key)
    cts[key] = 1 + get(cts, key, 0)
end

function step!(nums, npre, i, cts)
    for k in i-npre+1:i-1
        decr!(cts, nums[i - npre] + nums[k])
        incr!(cts, nums[i] + nums[k])
    end
end

function part1(nums, npre)
    cts = preamblecounts(nums, npre)
    for i in npre+1:length(nums)
        if !(nums[i] in keys(cts))
            return nums[i]
        end
        step!(nums, npre, i, cts)
    end
end

function cumulativesum(nums)
    out = zeros(Int, 1 + length(nums))
    for i in eachindex(nums)
        out[i+1] = out[i] + nums[i]
    end
    out
end

function findrangematchingsum(nums, target)
    cs = cumulativesum(nums)
    nc = length(cs)
    for l in 1:nc-1
        for r in l+1:nc
            diff = cs[r] - cs[l]
            if diff == target
                return (l, r-1)
            end
        end
    end
end

npre = 25
filename = "/home/xdavidliu/Documents/aoc/input09.txt"
nums = readnums(filename)
p1 = part1(nums, npre)
println("part 1 = ", p1)  # 90433990
(a, b) = findrangematchingsum(nums, p1)
p2 = minimum(nums[a:b]) + maximum(nums[a:b])
println("part 2 = ", p2)  # 11691646
