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

function playonce!(q1, q2)
    x1, x2 = pop!(q1), pop!(q2)
    if x1 > x2
        appendfronttwo!(q1, x2, x1)
    elseif x1 < x2
        appendfronttwo!(q2, x1, x2)
    else
        error("playonce")
    end
end

function score!(q)
    sum(i*x for (i, x) in enumerate(q))
end

function solve()
    filename = "/home/xdavidliu/Documents/input22.txt"
    text = read(filename, String)
    blocks = split(text, "\n\n")
    q1, q2 = blocknums(rstrip(blocks[1])), blocknums(rstrip(blocks[2]))
    reverse!(q1)
    reverse!(q2)
    i = 0
    while !isempty(q1) && !isempty(q2)
        playonce!(q1, q2)
        i += 1
    end
    win = isempty(q1) ? q2 : q1
    println("part 1 = ", score!(win))  # 32856
end

solve()
