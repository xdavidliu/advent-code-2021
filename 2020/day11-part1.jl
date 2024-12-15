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

function atleastfouroccupied(before, r, c)
    nocc = 0
    nr, nc = size(before)
    for dr in ds(r, nr)
        for dc in ds(c, nc)
            if dr == dc == 0
                continue
            elseif before[r+dr, c+dc] == '#'
                nocc += 1
                if nocc == 4
                    return true
                end
            end
        end
    end
    false
end

function noadjoccupied(before, r, c)
    nocc = 0
    nr, nc = size(before)
    for dr in ds(r, nr)
        for dc in ds(c, nc)
            if dr == dc == 0
                continue
            elseif before[r+dr, c+dc] == '#'
                return false
            end
        end
    end
    true
end

function nextseat(before, r, c)
    b = before[r, c]
    if b == 'L' && noadjoccupied(before, r, c)
        '#'
    elseif b == '#' && atleastfouroccupied(before, r, c)
        'L'
    else
        b
    end
end

function evolve(before, after)
    nr, nc = size(before)
    for r in 1:nr
        for c in 1:nc
            after[r, c] = nextseat(before, r, c)
        end
    end
end

lines = readlines("/home/xdavidliu/Documents/aoc/input11.txt")
before = charmatrix(lines)
nr, nc = size(before)
after = Matrix{Char}(undef, nr, nc);

while before != after
    evolve(before, after)
    before, after = after, before
end

p1 = count(==('#'), before, dims=:)
print("part 1 = ", p1)  # 2441
