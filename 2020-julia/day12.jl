function part1(pairs)
    dirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    dirpos = 1
    r = 0
    c = 0
    for (ch, n) in pairs
        if ch == 'F'
            dir = dirs[dirpos]
            r += n * dir[1]
            c += n * dir[2]
        elseif ch == 'E'
            c += n
        elseif ch == 'W'
            c -= n
        elseif ch == 'N'
            r -= n
        elseif ch == 'S'
            r += n
        elseif ch == 'R'
            k = div(n, 90)
            dirpos = (dirpos - 1 + k) % 4 + 1
        elseif ch == 'L'
            k = div(n, 90)
            dirpos = (dirpos - 1 - k + 4) % 4 + 1
        end
    end
    abs(r) + abs(c)
end

function part2(pairs)
    r, c = 0, 0
    wr, wc = -1, 10
    for (ch, n) in pairs
        if ch == 'F'
            r += n * wr
            c += n * wc
        elseif ch == 'E'
            wc += n
        elseif ch == 'W'
            wc -= n
        elseif ch == 'N'
            wr -= n
        elseif ch == 'S'
            wr += n
        elseif ch == 'R'
            k = div(n, 90)
            for _ in 1:k
                wr, wc = wc, -wr
            end
        elseif ch == 'L'
            k = div(n, 90)
            for _ in 1:k
                wr, wc = -wc, wr
            end
        end
    end
    abs(r) + abs(c)
end

filename = "/home/xdavidliu/Documents/aoc/input12.txt"
lines = readlines(filename)
pairs = [(s[1], parse(Int, s[2:end])) for s in lines]

p1 = part1(pairs)
p2 = part2(pairs)
println("part 1 = ", p1)  # 1710
println("part 2 = ", p2)  # 62045
