filename = "/home/xdavidliu/Documents/aoc/input17.txt"
grid = readlines(filename)
empty() = Set{Tuple{Int, Int, Int, Int}}()

function initialactive(grid)
    active = empty()
    for (r, row) in enumerate(grid)
        for (c, ch) in enumerate(row)
            if ch == '#'
                push!(active, (c-1, r-1, 0, 0))
            end
        end
    end
    active
end

function run(grid, dws)
    active = initialactive(grid)
    nextactive = empty()
    for _ in 1:6
        # number of active neighbors of inactive
        actnb = Dict{Tuple{Int, Int, Int, Int}, Int}()
        diffs = [-1, 0, 1]
        for (x, y, z, w) in active
            actcount = 0
            for dx in diffs
                for dy in diffs
                    for dz in diffs
                        for dw in dws
                            if 0 == dx == dy == dz == dw
                                continue
                            end
                            r = (x+dx, y+dy, z+dz, w+dw)
                            if r in active
                                actcount += 1
                            else
                                actnb[r] = get(actnb, r, 0) + 1
                            end
                        end
                    end
                end
            end
            if actcount == 2 || actcount == 3
                push!(nextactive, (x, y, z, w))
            end
        end
        for (r, c) in actnb
            # had bug here due to 3 == (1, 2) being legal in Julia
            if c == 3
                push!(nextactive, r)
            end
        end
        active, nextactive = nextactive, active
        empty!(nextactive)
    end
    length(active)
end

println("part 1 = ", run(grid, [0]))  # 211
println("part 2 = ", run(grid, [-1, 0, 1]))  # 1952
