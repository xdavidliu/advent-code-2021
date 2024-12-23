function run(nums, count)
    prev = -1
    turn = Dict{Int,Int}()
    say = 0
    for i in 1:count
        if i <= length(nums)
            say = nums[i]
            turn[prev] = i-1
            prev = say
        else
            t = get(turn, prev, -1)
            if t == -1 
                say = 0
            else
                say = i-1-t
            end
            turn[prev] = i-1
            prev = say
        end
    end
    return say
end

nums = [0,3,1,6,7,5]
println("part 1 = ", run(nums, 2020))  # 852
println("part 2 = ", run(nums, 30_000_000))  # 6007666
