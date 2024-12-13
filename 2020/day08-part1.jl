function parseline(line)
    words = split(line, " ")
    words[1], parse(Int, words[2])
end

function part1(ins)
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

filename = "/Users/xdavidliu/Documents/input08.txt"
ins = [parseline(l) for l in readlines(filename)]
p1, _ = part1(ins)
println("part 1 = ", p1)  # 1753
