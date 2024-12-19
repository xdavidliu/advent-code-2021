empty() = Vector{Int}()

function insertrule!(ruledict, line)
    lhs, rhs = split(line, ": ")
    ruledict[parse(Int, lhs)] = parseright(rhs)
end

function parseright(text)
    if text[1] == '"'
        text[2]
    else
        branches = split(text, " | ")
        [parsebranch(b) for b in branches]
    end
end

function parsebranch(b)
    [parse(Int, x) for x in split(b, " ")]
end

function matchsingle(text, startpos, ch)
    if text[startpos] == ch
        [startpos+1]
    else
        empty()
    end
end

function matchrule(text, startpos, ruledict, rule)
    if startpos == length(text)+1
        return empty()
    end
    rhs = ruledict[rule]
    if typeof(rhs) == Char
        return matchsingle(text, startpos, rhs)
    else
        out = empty()
        for branch in rhs
            ms = matchbranch(text, startpos, ruledict, branch)
            append!(out, ms)
        end
        return unique(out)
    end
end

function matchbranch(text, startpos, ruledict, branch)
    ms = [startpos]
    for br in branch
        mstemp = empty()
        for m in ms
            bms = matchrule(text, m, ruledict, br)
            append!(mstemp, bms)
        end
        ms = unique(mstemp)
    end
    ms
end

function isfullmatch(text, ruledict)
    ms = matchrule(text, 1, ruledict, 0)
    length(text)+1 in ms
end

function readproblem(filename)
    text = read(filename, String)
    blocks = split(text, "\n\n")
    rulelines = split(rstrip(blocks[1]), "\n")
    msglines = [String(s) for s in split(rstrip(blocks[2]), "\n")]
    ruledict = Dict{Int, Union{Char, Vector{Vector{Int}}}}()
    for line in rulelines
        insertrule!(ruledict, line)
    end
    ruledict, msglines
end

countmatch(ruledict, msgs) = sum(isfullmatch(m, ruledict) for m in msgs)

filename = "/home/xdavidliu/Documents/aoc/input19.txt"
ruledict, msgs = readproblem(filename)

println("part 1 = ", countmatch(ruledict, msgs))  # 111

ruledicttwo = copy(ruledict)
ruledicttwo[8] = [[42], [42, 8]]
ruledicttwo[11] = [[42, 31], [42, 11, 31]]

println("part 2 = ", countmatch(ruledicttwo, msgs))  # 343
