function getgrid(block)
    lines = split(block, "\n")
    n = parse(Int, lines[1][6:end-1])
    (n, [String(s) for s in lines[2:end]])
end

function readproblem(filename)
    blocks = split(rstrip(read(filename, String)), "\n\n")
    [getgrid(b) for b in blocks]
end

function edges(grid)
    [grid[1], grid[end], String([q[end] for q in grid]), String([q[1] for q in grid])]
end

function withreverse(lines)
    vcat(lines, [reverse(x) for x in lines])
end

function part1(gs)
    tab = Dict{String, Vector{Int}}()
    for (n, grid) in gs
        for ed in withreverse(edges(grid))
            a = get!(tab, ed, [])
            push!(a, n)
        end
    end
    count = Dict{Int, Int}()
    for a in values(tab)
        if 1 == length(a)
            count[a[1]] = get(count, a[1], 0) + 1
        end
    end
    p1 = 1
    for (k, c) in count
        if c == 4
            p1 *= k
        end
    end
    p1
end

filename = "/home/xdavidliu/Documents/aoc/input20.txt"
gs = readproblem(filename);
println("part 1 = ", part1(gs))  # 28057939502729
