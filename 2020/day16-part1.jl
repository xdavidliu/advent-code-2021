function insertrange(line, ranges)
    re = r".+: (\d+)-(\d+) or (\d+)-(\d+)"
    mch = match(re, line)
    ns = [parse(Int, mch[i]) for i in 1:4]
    push!(ranges, (ns[1], ns[2]))
    push!(ranges, (ns[3], ns[4]))
end

function within(ranges, x)
    for (a, b) in ranges
        if a <= x <= b
            return true
        end
    end
    false
end

function getinvalid(blocks)
    ranges = Vector{Tuple{Int, Int}}()
    for line in split(strip(blocks[1]), "\n")
        insertrange(line, ranges)
    end
    inv = Vector{Int}()
    for line in split(strip(blocks[3]), "\n")[2:end]
        for word in split(line, ",")
            k = parse(Int, word)
            if !within(ranges, k)
                push!(inv, k)
            end
        end
    end
    inv
end

filename = "/usr/local/google/home/xdavidliu/Documents/temp/input16.txt"
text = read(filename, String)
blocks = split(text, "\n\n")
inv = getinvalid(blocks)
println("part 1 = ", sum(inv))  # 29851
