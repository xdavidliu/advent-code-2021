function parselistitem(text)  # "4 vibrant aqua bags"
    re = r"^(\d)+ (\w+ \w+) bags?$"
    mch = match(re, text)
    parse(Int, mch[1]), String(mch[2])
end

function parseline(line)
    # chop for last period
    bag, containtext = split(chop(line), " bags contain ")
    containlist = containtext == "no other bags" ?
        [] : [parselistitem(s) for s in split(containtext, ", ")]
    String(bag), containlist
end

getentries(filename) = [parseline(x) for x in readlines(filename)]

function populatetable(entries)
    tab = Dict{String, Array{String}}()
    for (outer, items) in entries
        for (_, inner) in items
            lst = get!(tab, inner, [])
            push!(lst, outer) 
        end
    end
    tab
end

function dfs1(bag, table, seen)
    for other in get(table, bag, [])
        push!(seen, other)
        dfs1(other, table, seen)
    end
end

function part1()
    entries = getentries("/Users/xdavidliu/Documents/input07.txt")
    tab = populatetable(entries)
    seen = Set{String}()
    dfs1("shiny gold", tab, seen)
    println("part 1 = ", length(seen))
end

function dfs2(bag, entriesdict)
    total = 0
    ps = get(entriesdict, bag, [])
    for (c, other) in ps
        total += c * dfs2(other, entriesdict)
    end
    total + 1  # for bag itself
end

function part2()
    entries = getentries("/Users/xdavidliu/Documents/input07.txt")
    entriesdict = Dict(entries)
    tab = populatetable(entries)
    # -1 to exclude shiny gold itself
    println("part 2 = ", -1 + dfs2("shiny gold", entriesdict))
end

part1()  # 192
part2()  # 12128
