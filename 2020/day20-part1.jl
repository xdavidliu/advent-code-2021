function tomatrix(lines)
    mat = Matrix{Char}(undef, length(lines), length(lines[1]))
    for (r, row) in enumerate(lines)
        for (c, val) in enumerate(row)
            mat[r, c] = val
        end
    end     
    mat
end

displaycharmatrix(m) = println(join([String(m[i,:]) for i in 1:size(m, 1)], "\n"))

function getgrid(block)
    lines = split(block, "\n")
    n = parse(Int, lines[1][6:end-1])
    (n, tomatrix(lines[2:end]))
end

function readproblem(filename)
    blocks = split(rstrip(read(filename, String)), "\n\n")
    Dict(getgrid(b) for b in blocks)
end

function edges(m)
    # clockwise starting with left edge
    p = [m[end:-1:1,1], m[1,:], m[:, end], m[end, end:-1:1]]
    [String(x) for x in p]
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

function getupperleft(tab, count)
    tile = first(k for (k, c) in count if c == 4)
    g = gs[tile]
    for _ in 1:4
        if [length(tab[x]) for x in edges(g)] == [2, 2, 1, 1]
            return g
        else
            g = rotr90(g)
        end
    end
    error("getupperleft")
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


# find first 2: that one is the left edge and next one is upper edge
# the second 2 can be wrapped around
# stamp that down.

# wait, just rotate it until edges are 2 2 1 1

# okay, convert to matrix. Can use julia rot90!
