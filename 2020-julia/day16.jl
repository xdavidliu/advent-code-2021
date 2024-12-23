function insertrange(line, ranges)
    re = r"(.+): (\d+)-(\d+) or (\d+)-(\d+)"
    mch = match(re, line)
    ns = [parse(Int, mch[i]) for i in 2:5]
    ranges[mch[1]] = (ns[1], ns[2], ns[3], ns[4])
end

within(v, x) = v[1] <= x <= v[2] || v[3] <= x <= v[4] 
withinany(ranges, x) = any(within(v, x) for v in values(ranges))

function getranges(block)
    ranges = Dict{String, Tuple{Int, Int, Int, Int}}()
    for line in split(block, "\n")
        insertrange(line, ranges)
    end
    ranges
end

function getinvalid(blocks, ranges, nearby)
    inv = Set{Int}()
    total = 0
    for row in nearby
        for k in row
            if !withinany(ranges, k)
                total += k
                push!(inv, k)
            end
        end
    end
    total, inv
end

function parseNearby(block)
    lines = split(strip(block), "\n")[2:end]  # ignore "nearby tickets" title
    [[parse(Int, w) for w in split(ln, ",")] for ln in lines]
end

function deduce(ranges, invset, nearby)
    n = length(nearby[1])
    names = Set(keys(ranges))
    sets = [copy(names) for _ in 1:n]
    # perf optimization to avoid allocing inside
    todelete = repeat([""], length(names))
    delcount = 0
    for row in nearby
        @assert n == length(row)
        for i in 1:n
            if row[i] in invset
                continue
            end
            for x in sets[i]
                if !within(ranges[x], row[i])
                    delcount += 1
                    todelete[delcount] = x
                end
            end
            for k in 1:delcount
                delete!(sets[i], todelete[k])
            end
            delcount = 0
        end
    end
    return deducesets(sets)
end

function deducesets(sets)
    n = length(sets)
    answers = repeat([""], n)
    found = 0
    while found < n
        next = iterate(i for i in 1:n if length(sets[i]) == 1)
        if next != nothing
            (i, _) = next
            x = first(sets[i])
            answers[i] = x
            for s in sets
                delete!(s, x)
            end
            found += 1
        else
            println("ended prematurely")
        end
    end
    answers
end

function solve()
    filename = "/home/xdavidliu/Documents/aoc/input16.txt"
    text = read(filename, String)
    blocks = split(text, "\n\n")
    ranges = getranges(strip(blocks[1]))
    nearby = parseNearby(blocks[3])
    p1, invset = getinvalid(blocks, ranges, nearby)
    println("part 1 = ", p1)  # 29851
    mine = parseNearby(blocks[2])[1]  # hack
    answers = deduce(ranges, invset, nearby)
    p2 = 1
    for (x, s) in zip(mine, answers)
        if startswith(s, "departure")
            p2 *= x
        end
    end
    println("part 2 = ", p2)  # 3029180675981
end

solve()
