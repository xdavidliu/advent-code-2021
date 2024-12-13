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
    bag, containlist
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

function dfs(bag, table, seen)
    for other in get(table, bag, [])
        push!(seen, other)
        dfs(other, table, seen)
    end
end

function part1()
    entries = getentries("/Users/xdavidliu/Documents/input07.txt")
    tab = populatetable(entries)
    seen = Set{String}()
    dfs("shiny gold", tab, seen)
    println("part 1 = ", length(seen))
end

part1()  # 192
