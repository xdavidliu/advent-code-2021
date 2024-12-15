# https://discourse.julialang.org/t/converting-a-array-of-strings-to-an-array-of-char/35123/2
function charmatrix(lines)
    m = Matrix{Char}(undef, length(lines), length(lines[1]))
    for r in eachindex(lines)
        for c in eachindex(lines[r])
            m[r, c] = lines[r][c]
        end
    end
    m
end

function ds(i, n)
    if i == 1
        [0, 1]  # not sure if this CONSes
    elseif i == n
        [-1, 0]
    else
        [-1, 0, 1]
    end
end

function atleastfouroccupied(grid, r, c)
    nocc = 0
    nr, nc = size(grid)
    for dr in ds(r, nr)
        for dc in ds(c, nc)
            if dr == dc == 0
                continue
            elseif grid[r+dr, c+dc] == '#'
                nocc += 1
                if nocc == 4
                    return true
                end
            end
        end
    end
    false
end

function noadjoccupied(grid, r, c)
    nocc = 0
    nr, nc = size(grid)
    for dr in ds(r, nr)
        for dc in ds(c, nc)
            if dr == dc == 0
                continue
            elseif grid[r+dr, c+dc] == '#'
                return false
            end
        end
    end
    true
end

function nextseat(grid, r, c)
    b = grid[r, c]
    if b == 'L' && noadjoccupied(grid, r, c)
        '#'
    elseif b == '#' && atleastfouroccupied(grid, r, c)
        'L'
    else
        b
    end
end

function evolve!(before, after)
    nr, nc = size(before)
    for r in 1:nr
        for c in 1:nc
            after[r, c] = nextseat(before, r, c)
        end
    end
end

function part1(lines)
    before = charmatrix(lines)
    nr, nc = size(before)
    after = Matrix{Char}(undef, nr, nc);
    while before != after
        evolve!(before, after)
        before, after = after, before
    end
    p1 = count(==('#'), before, dims=:)
    println("part 1 = ", p1)  # 2441
end

function nextseattwo(grid, seen, r, c)
    b = grid[r, c]
    if b == 'L' && seen[r, c] == 0
        '#'
    elseif b == '#' && seen[r, c] >= 5
        'L'
    else
        b
    end
end

function evolvetwo!(before, after, seen)
    nr, nc = size(before)
    for r in 1:nr
        for c in 1:nc
            after[r, c] = nextseattwo(before, seen, r, c)
        end
    end
end

goright((r, c), _) = (r, c+1)
goleft((r, c), _) = (r, c-1)
goup((r, c), _) = (r-1, c)
godown((r, c), _) = (r+1, c)
gosoutheast((r, c), _) = (r+1, c+1)
gosouthwest((r, c), _) = (r+1, c-1)
gonortheast((r, c), _) = (r-1, c+1)
gonorthwest((r, c), _) = (r-1, c-1)

goupthenleft(p, nc) = p[1] == 1 ? goleft(p, nc) : goup(p, nc)
goupthenright(p, nc) = p[1] == 1 ? goright(p, nc) : goup(p, nc)
goleftthenup(p, nc) = p[2] == 1 ? goup(p, nc) : goleft(p, nc)
# lol this is the only one that needs the other param
gorightthenup(p, nc) = p[2] == nc ? goup(p, nc) : goright(p, nc)

function isinside(grid, (r, c))
    nr, nc = size(grid)
    1 <= r <= nr && 1 <= c <= nc
end

function scan!(dest, grid, start, updatestart, updateinner)
    _, nc = size(grid)
    while isinside(grid, start)
        inner = start
        last = false
        while isinside(grid, inner)
            # set dest before because occupied does not see itself
            dest[inner[1], inner[2]] += last
            g = grid[inner[1], inner[2]]
            if g == '#'
                last = true
            elseif g == 'L'
                # empty seats block occupied
                last = false
            end
            inner = updateinner(inner, nc)
        end
        start = updatestart(start, nc)
    end
end

function scanall!(seen, grid)
    nr, nc = size(grid)
    fill!(seen, 0)
    scan!(seen, grid, (nr, 1), goupthenright, gosoutheast)
    scan!(seen, grid, (nr, nc), goleftthenup, gonortheast)
    scan!(seen, grid, (nr, nc), goupthenleft, gosouthwest)
    scan!(seen, grid, (nr, 1), gorightthenup, gonorthwest)
    scan!(seen, grid, (1, 1), goright, godown)
    scan!(seen, grid, (nr, 1), goright, goup)
    scan!(seen, grid, (1, 1), godown, goright)
    scan!(seen, grid, (1, nc), godown, goleft)
end

function showgrid(grid)  # for debug
    nr, nc = size(grid)
    for r in 1:nr
        println(String(grid[r,:]))
    end
    println()
end

function part2(lines)
    before = charmatrix(lines)
    nr, nc = size(before)
    after = Matrix{Char}(undef, nr, nc);
    seen = zeros(Int, nr, nc);
    while before != after
        scanall!(seen, before)
        evolvetwo!(before, after, seen)
        before, after = after, before
        # showgrid(before)
    end
    p2 = count(==('#'), before, dims=:)
    println("part 2 = ", p2)  # 2190
end

lines = readlines("/home/xdavidliu/Documents/aoc/input11.txt")
part1(lines)
part2(lines)
