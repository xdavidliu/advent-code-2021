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
end

solve()
