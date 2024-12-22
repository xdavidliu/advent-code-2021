function appendfronttwo!(a, x1, x2)
    push!(a, 0)
    push!(a, 0)
    for i in reverse(3:length(a))
        a[i] = a[i-2]
    end
    a[1] = x1
    a[2] = x2
end

function blocknums(bl)
    lines = split(bl, "\n")
    [parse(Int, x) for x in lines[2:end]]
end

function playonce!(a1, a2, rec)
    win = 0
    x1, x2 = pop!(a1), pop!(a2)
    if rec && length(a1) >= x1 && length(a2) >= x2
        win = startrecursive!(a1[end-x1+1:end], a2[end-x2+1:end])
    elseif x1 > x2
        win = 1
    elseif x1 < x2
        win = 2 
    else
        error("playonce")
    end
    if win == 1
        appendfronttwo!(a1, x2, x1)
    else
        appendfronttwo!(a2, x1, x2)
    end 
end

serialize(a1, a2) = join([a1, a2], ";")

function startrecursive!(a1, a2)
    seen = Set{String}()
    while !isempty(a1) && !isempty(a2)
        before = serialize(a1, a2)
        if before in seen
            return 1
        end
        push!(seen, before)
        playonce!(a1, a2, true)
    end
    isempty(a1) ? 2 : 1
end

function score(q)
    sum(i*x for (i, x) in enumerate(q))
end

function readproblem(filename)
    text = read(filename, String)
    blocks = split(text, "\n\n")
    a1, a2 = blocknums(rstrip(blocks[1])), blocknums(rstrip(blocks[2]))
    reverse!(a1)
    reverse!(a2)
    a1, a2
end

function part1(a1, a2)
    a1 = copy(a1)
    a2 = copy(a2)
    while !isempty(a1) && !isempty(a2)
        playonce!(a1, a2, false)
    end
    win = isempty(a1) ? a2 : a1
    score(win)
end

function part2(a1, a2)
    a1 = copy(a1)
    a2 = copy(a2)
    win = startrecursive!(a1, a2) == 1 ? a1 : a2
    score(win)
end

function solve()
    filename = "/home/xdavidliu/Documents/input22.txt"
    a1, a2 = readproblem(filename)
    println("part 1 = ", part1(a1, a2))  # 32856
    println("part 2 = ", part2(a1, a2))  # 33805
end

solve()
