function linesfromfile(filename)
    open(filename) do f
        readlines(f)
    end
end

function counttrees(lines, right, down)
    col = 1
    ncol = length(lines[1])
    tc = 0
    # https://discourse.julialang.org/t/eachindex-x-replacement-for-1-length-x-1-and-similar/81481/7
    for r in firstindex(lines):down:lastindex(lines)
        tc += lines[r][col] == '#'
        col = (col-1+right) % ncol + 1
    end
    tc
end

filename = "/home/xdavidliu/Documents/aoc/input03.txt"
lines = linesfromfile(filename)
f(r, d) = counttrees(lines, r, d)
p1 = f(3, 1)
println("part 1 = ", p1)  # 189
p2 = f(1, 1) * p1 * f(5, 1) * f(7, 1) * f(1, 2)
println("part 2 = ", p2)  # 1718180100
