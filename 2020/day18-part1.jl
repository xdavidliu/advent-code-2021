function evalint(text, pos)
    k = findnext(r"[^0-9]", text, pos)
    if k == nothing
        k = length(text)
    else
        k = k[1]  # because it returns a range like 3:3
    end
    parse(Int, text[pos:k-1]), k
end

# why does julia allow 'a' == "a"
function opint(op)
    if op == '+'
        -1
    elseif op == '*'
        -2
    else
        error("opint" )
    end
end

function evalcompound(text, pos, collapse)
    @assert text[pos] == '('
    i = pos
    ops = Vector{Int}()
    while text[i] != ')'
        v, k = evalexpr(text, i+1, collapse)
        if text[i] == '('
            push!(ops, v)
        else
            push!(ops, opint(text[i]))
            if v == nothing
                println(text, "\n", pos)
                println(i)
            end
            push!(ops, v)
        end
        i = k
    end
    return collapse(ops), i+1
end

function evalexpr(text, pos, collapse)
    if isdigit(text[pos])
        evalint(text, pos)
    else
        evalcompound(text, pos, collapse)
    end
end

function evaltoplevel(text, collapse)
    s = "(" * replace(text, " " => "") * ")"  # awkward, * not +?
    evalexpr(s, 1, collapse)[1]
end

function collapseone(ops)
    acc = ops[1]
    i = 2
    while i <= length(ops)
        v = ops[i+1]
        op = ops[i]
        if op == -1
            acc += v
        else
            acc *= v
        end
        i += 2
    end
    acc
end

filename = "/home/xdavidliu/Documents/aoc/input18.txt"
p1 = sum(evaltoplevel(s, collapseone) for s in readlines(filename))
println("part 1 = ", p1)  # 12918250417632
