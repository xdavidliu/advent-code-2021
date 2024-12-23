filename = "/home/xdavidliu/Documents/aoc/input13.txt"
lines = readlines(filename)
depart = parse(Int, lines[1])
idwords = split(lines[2], ",")
ids = [id == "x" ? -1 : parse(Int, id) for id in idwords]

function part1(depart, ids)
    best = typemax(Int)
    bestid = -1
    for id in ids
        if id == -1
            continue
        end
        val = id - depart % id
        if val < best
            bestid = id
            best = val
        end
    end
    best * bestid
end

# taken from 2019 day 22, also see 11-27-aoc-notes.txt
function modinverse(a, b)
    z(x, y) = a * x + b * y
    x1, y1 = 1, 0
    x2, y2 = 0, 1
    while z(x2, y2) != 0
        q = div(z(x1, y1), z(x2, y2))
        x3 = x1 - q * x2
        y3 = y1 - q * y2
        x1, y1 = x2, y2
        x2, y2 = x3, y3        
    end
    mod(y1, a)
end

function part2(ids)
    # CLRS 4ed 31.5 Chinese Remainder Thm
    ns = Vector{Int}()
    as = Vector{Int}()
    for off in eachindex(ids)
        id = ids[off]
        if id == -1
            continue
        end
        push!(ns, id)
        # note mod is negative of the number of minutes
        push!(as, id - off + 1)
    end
    n = prod(ns)
    ms = [div(n, k) for k in ns]
    cs = [m * modinverse(ni, m) for (m, ni) in zip(ms, ns)]
    a = sum(mod(ai * c, n) for (ai, c) in zip(as, cs))
    mod(a, n)
end

p1 = part1(depart, ids)
p2 = part2(ids)
println("part 1 = ", p1)  # 2995
println("part 2 = ", p2)  # 1012171816131114
