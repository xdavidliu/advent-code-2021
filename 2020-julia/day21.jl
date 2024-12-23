function getsets(s)
    left, right = split(s, " (contains ")
    ingred = Set(split(left, " "))
    alleg = Set(split(right[1:end-1], ", "))
    (ingred, alleg)
end

function readproblem(filename)
    lines = readlines(filename)
    pairs = [getsets(x) for x in lines]
    ings = [p[1] for p in pairs]
    alls = [p[2] for p in pairs]
    ings, alls
end

function getknown(ings, alls)
    poss = Dict{String, Vector{Set{String}}}()
    for (ix, ax) in zip(ings, alls)
        for x in ax
            g = get!(poss, x, [])
            push!(g, ix)
        end
    end
    red = Dict{String, Set{String}}()
    for (k, v) in poss
        red[k] = reduce(intersect, v)
    end
    known = Set{String}()
    knownvalue = Dict{String, String}()
    i = 0
    while length(known) < length(poss)
        i += 1
        if i == 1000
            println("limit reached")
            break
        end
        s = ""
        for (k, v) in red
            if length(v) == 1
                s = first(v)
                push!(known, s)
                knownvalue[k] = s
                found = true
                break
            end
        end
        if !isempty(s)
            for (_, w) in red
                delete!(w, s)
            end
        end
    end
    known, knownvalue
end

function part1(ings, known)
    p = 0
    for s in ings
        p += length(setdiff(s, known))
    end
    p
end

function solve()
    ings, alls = readproblem("/home/xdavidliu/Documents/input21.txt")
    known, knownvalue = getknown(ings, alls)
    p1 = part1(ings, known)
    println("part 1 = ", p1)  # 2162
    alpha = sort([k for k in keys(knownvalue)])
    p2 = join([knownvalue[x] for x in alpha], ",")
    println("part 2 = ", p2)  # lmzg,cxk,bsqh,bdvmx,cpbzbx,drbm,cfnt,kqprv
end

solve()
