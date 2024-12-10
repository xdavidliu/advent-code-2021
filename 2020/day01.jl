a = open("/home/xdavidliu/Documents/aoc/input01.txt") do f
    [parse(Int, s) for s in readlines(f)]
end

function partone(a)
    for i in eachindex(a)
        for k = i+1:length(a)
            if a[i] + a[k] == 2020
                return a[i] * a[k]
            end
        end
    end
    error()
end

function parttwo(a)
    for i in eachindex(a)
        for j = i+1:length(a)-1
            for k = j+1:length(a)
                if a[i] + a[j] + a[k] == 2020
                    return a[i] * a[j] * a[k]
                end
            end
        end
    end
end

println("part 1 = ", partone(a))  # 355875
println("part 2 = ", parttwo(a))  # 140379120
