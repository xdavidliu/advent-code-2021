text = read("/home/xdavidliu/Documents/aoc/input06.txt", String)
blocks = split(text, "\n\n");
score(b, op) = length(reduce(op, [Set(x) for x in split(strip(b), "\n")]))
println("part 1 = ", sum(score(b, union) for b in blocks))  # 6530
println("part 2 = ", sum(score(b, intersect) for b in blocks))  # 3323
