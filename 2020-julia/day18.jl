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

function optotal(ops)
    sum(ops[1:2:end])
end

function collapsetwo(ops)
    acc = one(Int128)
    inds = findall(x -> x == -2, ops)
    push!(inds, length(ops)+1)  # sentinel * at one past end
    l = 0
    for r in inds
        acc *= optotal(ops[l+1:r-1])
        l = r
    end
    acc
end

function solve()
    filename = "/home/xdavidliu/Documents/aoc/input18.txt"
    lines = readlines(filename)
    p1 = sum(evaltoplevel(s, collapseone) for s in lines)
    println("part 1 = ", p1)  # 12918250417632
    p2 = sum(evaltoplevel(s, collapsetwo) for s in lines)
    println("part 2 = ", p2)  # 171259538712010
end

solve()
