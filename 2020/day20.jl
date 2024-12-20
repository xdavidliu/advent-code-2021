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

function edges(m, i)
    # clockwise starting with left edge
    z = if i == 1
        m[end:-1:1,1]
    elseif i == 2
        m[1,:]
    elseif i == 3
        m[:, end]
    elseif i == 4
        m[end, end:-1:1]
    else
        error("edges i")
    end
    String(z)
end

edges(m) = [edges(m, i) for i in 1:4]

function withreverse(lines)
    vcat(lines, [reverse(x) for x in lines])
end

function getedge2tile(gs)
    edge2tile = Dict{String, Vector{Int}}()
    for (n, grid) in gs
        for ed in withreverse(edges(grid))
            a = get!(edge2tile, ed, [])
            push!(a, n)
        end
    end
    edge2tile
end

function getsingletilecount(edge2tile)
    singletilecount = Dict{Int, Int}()
    for a in values(edge2tile)
        if 1 == length(a)
            singletilecount[a[1]] = get(singletilecount, a[1], 0) + 1
        end
    end
    singletilecount
end

function getupperleft(tab, count, gs)
    tile = first(k for (k, c) in count if c == 4)
    g = gs[tile]
    for _ in 1:4
        if [length(tab[x]) for x in edges(g)] == [1, 1, 2, 2]
            return tile, g
        else
            g = rotr90(g)
        end
    end
    error("getupperleft")
end

function matchedge(m1, m2, i1, i2)
    e1 = edges(m1, i1)
    if e1 in edges(m2)
        # because when matching, the edge on right will be reversed
        # when going clockwise
        m2 = reverse(m2, dims=2)
    end
    re1 = reverse(e1)
    for _ in 1:4
        if edges(m2, i2) == re1
            return m2
        end
        m2 = rotr90(m2)
    end
    error("matchedge")
end

matchhoriz(m1, m2) = matchedge(m1, m2, 3, 1)
matchvert(m1, m2) = matchedge(m1, m2, 4, 2)

function getothertile(edge2tile, m1, t1, i)
    ts = edge2tile[edges(m1, i)]
    t1 == ts[1] ? ts[2] : ts[1]
end

gettileright(edge2tile, m1, t1) = getothertile(edge2tile, m1, t1, 3)
gettilebelow(edge2tile, m1, t1) = getothertile(edge2tile, m1, t1, 4)

function assembletiles(gs, edge2tile, singletilecount)
    nblk = isqrt(length(gs))
    npt = length(first(keys(edge2tile)))-2
    arr = Matrix{Char}(undef, nblk * npt, nblk * npt);
    t1, m1 = getupperleft(edge2tile, singletilecount, gs)
    arr[1:npt, 1:npt] = m1[2:end-1,2:end-1]
    for r in 1:nblk
        t, m = t1, m1
        for c in 2:nblk
            t2 = gettileright(edge2tile, m, t)
            m2 = matchhoriz(m, gs[t2])
            arr[(r-1)*npt+1:r*npt, (c-1)*npt+1:c*npt] = m2[2:end-1, 2:end-1]
            t, m = t2, m2
        end
        if r < nblk
            tb = gettilebelow(edge2tile, m1, t1)
            mb = matchvert(m1, gs[tb])
            arr[r*npt+1:(r+1)*npt, 1:npt] = mb[2:end-1, 2:end-1]
            t1, m1 = tb, mb
        end
    end
    arr
end

function presentindices(monster)
    inds = Vector{Tuple{Int, Int}}()
    for r in 1:size(monster, 1)
        for c in 1:size(monster, 2)
            if monster[r, c] == '#'
                push!(inds, (r, c))
            end
        end
    end
    inds
end

function ismonster(arr, r, c, indices)
    for (dr, dc) in indices
        if arr[r+dr-1, c+dc-1] != '#'
            return false
        end
    end
    true
end

function countmonster(arr, monster, indices)
    nr, nc = size(monster)
    k = 0
    for r in 1:size(arr,1)+1-size(monster,1)
        for c in 1:size(arr,2)+1-size(monster,2)
            if ismonster(arr, r, c, indices)
                k += 1
            end
        end
    end
    k
end

function countallmonsters(arr, originalmonster)
    for m in alleight(originalmonster)
        indices = presentindices(m)
        k = countmonster(arr, m, indices)
        if k != 0
            return k
        end
    end
    error("countallmonsters")
end

sameshapetrans(a) = [a, reverse(a), reverse(a, dims=1), reverse(a, dims=2)]
# not transpose
# https://discourse.julialang.org/t/error-methoderror-no-method-matching-adjoint-datetime/68230/3
alleight(a) = vcat(sameshapetrans(a), sameshapetrans(permutedims(a)))

function solve()
    filename = "/usr/local/google/home/xdavidliu/Documents/input20.txt"
    gs = readproblem(filename);
    edge2tile = getedge2tile(gs)
    singletilecount = getsingletilecount(edge2tile)
    p1 = prod(k for (k, c) in singletilecount if c == 4)
    println("part 1 = ", p1)  # 28057939502729
    arr = assembletiles(gs, edge2tile, singletilecount)

    monstertext = 
"                  # 
#    ##    ##    ###
 #  #  #  #  #  #   "

    monster = tomatrix(split(monstertext, "\n"))    
    moncount = countallmonsters(arr, monster)
    arrhashcount = count(x -> x=='#', arr)
    monhashcount = count(x -> x=='#', monster)
    p2 = arrhashcount - moncount * monhashcount
    println("part 2 = ", p2)  # 2489
end

arr = solve();
