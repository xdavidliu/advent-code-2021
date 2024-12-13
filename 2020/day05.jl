function bitsum(s, z)
    x = 0
    for c in s
        x <<= 1
        x += (c == z)
    end
    x
end

function rowcol(s)
    r = bitsum(s[1:7], 'B')
    c = bitsum(s[8:end], 'R')
    (r, c)
end

id((r, c)) = 8 * r + c
rc(i) = (div(i,8), i % 8)

lines = readlines("/home/xdavidliu/Documents/aoc/input05.txt");
ids = Set(id(rowcol(s)) for s in lines)
println("part 1 = ", maximum(ids))  # 880

function find(ids)
    for i in ids
        if i+2 in ids && !(i+1 in ids)
            (r, c) = rc(i+1)
            if 0 < r < 127
                return i+1
            end
        end
    end
end
println("part 2 = ", find(ids))  # 731
