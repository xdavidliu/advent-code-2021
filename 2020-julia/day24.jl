horiz(ch) = ch == 'e' ? 1 : -1
vert(ch) = ch == 'n' ? 1 : -1

function step(text, pos, x, y)
    ch = text[pos]
    pos2 = 0
    r = (0, 0)
    if ch == 'e' || ch == 'w'
        pos + 1, (x+2*horiz(ch), y)
    else
        pos + 2, (x+horiz(text[pos+1]), y+vert(ch))
    end
end

function stepall(text)
    x, y = 0, 0
    pos = 1
    while pos <= length(text)
        pos, (x, y) = step(text, pos, x, y)
    end
    x, y
end

neighbors(r) = [(r[1]+dx, r[2]+dy) for (dx, dy) in [(-2, 0), (2, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]]

function iter!(black)
    nbofwhite = Dict{Tuple{Int, Int}, Int}()
    b2w = Set{Tuple{Int, Int}}()
    for r0 in black
        blknb = 0
        for r in neighbors(r0)
            if r in black
                blknb += 1
            else
                nbofwhite[r] = get(nbofwhite, r, 0) + 1
            end
        end
        if blknb == 0 || blknb > 2
            push!(b2w, r0)
        end
    end
    for r in b2w
        delete!(black, r)
    end
    for (r, c) in nbofwhite
        if c == 2
            push!(black, r)
        end
    end
end

function solve()
    filename = "/home/xdavidliu/Documents/input24.txt"
    lines = readlines(filename)
    black = Set{Tuple{Int, Int}}()
    for line in lines
        r = stepall(line)
        if r in black
            delete!(black, r)
        else
            push!(black, r)
        end
    end
    println("part 1 = ", length(black))  # 455
    for _ in 1:100
        iter!(black)
    end
    println("part 2 = ", length(black))  # 3904
end

solve()
