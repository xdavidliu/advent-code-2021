function parseline(line)
    words = split(line, " ")
    words[1], parse(Int, words[2])
end

function run(ins)
    acc = 0
    seen = repeat([false], length(ins))
    ptr = 1
    while ptr in eachindex(ins)
        if seen[ptr]
            return acc, false
        else
            seen[ptr] = true
        end
        name, off = ins[ptr]
        if name == "acc"
            acc += off
            ptr += 1
        elseif name == "jmp"
            ptr += off
        else
            ptr += 1
        end
    end
    return acc, true
end

function flip(ins, k)
    name, c = ins[k]
    if name == "nop"
        ins[k] = ("jmp", c)
    elseif name == "jmp"
        ins[k] = ("nop", c)
    end
end

function part2(ins)
    for k in eachindex(ins)
        flip(ins, k)
        acc, termed = run(ins)
        flip(ins, k)
        if termed
            return acc
        end
    end
end

filename = "/Users/xdavidliu/Documents/input08.txt"
ins = [parseline(l) for l in readlines(filename)]
p1, _ = run(ins)
println("part 1 = ", p1)  # 1753
println("part 2 = ", part2(ins))  # 733
