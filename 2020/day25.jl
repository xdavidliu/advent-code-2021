# y1 = 17807724
# y2 = 5764801
unknown = -1

update(x, sbj) = (x * sbj) % 20201227

function search(x1, x2)
    y1, y2 = 1, 1
    lp1, lp2 = unknown, unknown
    i = 0
    sbj = 7
    while lp1 == unknown || lp2 == unknown
        i += 1
        if lp1 == unknown
            y1 = update(y1, sbj)
            if y1 == x1
                lp1 = i
            end
        end
        if lp2 == unknown
            y2 = update(y2, sbj)
            if y2 == x2
                lp2 = i
            end
        end
    end
    lp1, lp2
end

function transform(sbj, lp)
    x = 1
    for _ in 1:lp
        x = update(x, sbj)
    end
    x
end

function solve(x1, x2)
    lp1, lp2 = search(x1, x2)
    transform(x2, lp1)
end

# solve(17807724, 5764801)
p1 = solve(13233401, 6552760)  # 17673381
println("part 1 = ", p1)
