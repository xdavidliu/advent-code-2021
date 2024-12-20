function getgrid(block)
    lines = split(block, "\n")
    n = parse(Int, lines[1][6:end-1])
    (n, [String(s) for s in lines[2:end]])
end

function readproblem(filename)
    blocks = split(rstrip(read(filename, String)), "\n\n")
    Dict(getgrid(b) for b in blocks)
end

function column(grid, right)
    right ? String([q[end] for q in grid]) : String([q[1] for q in grid])
end

function edges(grid)
    # use reverses to make these clockwise
    [grid[1], column(grid, true), reverse(grid[end]), reverse(column(grid, false))]
end

function withreverse(lines)
    vcat(lines, [reverse(x) for x in lines])
end

function getdicts(gs)
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
    tab, count
end

filename = "/home/xdavidliu/Documents/aoc/sample.txt"
gs = readproblem(filename);
tab, count = getdicts(gs)
p1 = prod(k for (k, c) in count if c == 4)
println("part 1 = ", p1)  # 28057939502729

nblk = isqrt(length(gs))
npt = length(first(keys(tab)))
arr = Matrix{Char}(undef, nblk * npt, nblk * npt);
# using count, get one of the four corner ones, and put it in upper left corner
# using tab, fill the upper row
# keep filling more rows until whole thing is filled
# take away the pizza crusts, create another smaller arr
# create pair stencil for sea monster, try string matching all 8 directions
# one of them will have the most. Hopefully other 7 have zero.

upperleft = first(k for (k, c) in count if c == 4)
[length(tab[x]) for x in edges(gs[upperleft])]  # one for outer-facing

# find first 2: that one is the left edge and next one is upper edge
# the second 2 can be wrapped around
# stamp that down.
