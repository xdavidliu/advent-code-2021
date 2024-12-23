function countvalid(filename)
    open(filename) do f
        lines = readlines(f)
        ps = [isvalid(line) for line in lines]
        p1 = sum(p[1] for p in ps)
        p2 = sum(p[2] for p in ps)
        p1, p2
    end
end

function isvalidone(low, high, symbol, text)
    low <= count(symbol, text) <= high
end

function isvalidtwo(n1, n2, symbol, text)
    s1 = string(text[n1])
    s2 = string(text[n2])
    (s1 == symbol) != (s2 == symbol)
end

function isvalid(line)
    re = r"^(\d+)-(\d+) (\w): (\w+)$"
    m = match(re, line)
    low = parse(Int, m[1])
    high = parse(Int, m[2])
    p1 = isvalidone(low, high, m[3], m[4])
    p2 = isvalidtwo(low, high, m[3], m[4])
    (p1, p2)
end

filename = "/home/xdavidliu/Documents/aoc/input02.txt"
(p1, p2) = countvalid(filename)
println("part 1 = ", p1)  # 439
println("part 2 = ", p2)  # 584
