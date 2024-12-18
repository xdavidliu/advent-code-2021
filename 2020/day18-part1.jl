# remember to replace space
function evalexpr(text, pos)
    if isdigit(text[pos])
        k = findnext(r"[^0-9]", text, pos)
        if k == nothing
            k = length(text)
        else
            k = k[1]  # because it returns a range like 3:3
        end
        return parse(Int, text[pos:k-1]), k
    else
        @assert text[pos] == '('
        i = pos
        acc = 0
        while text[i] != ')'
            v, k = evalexpr(text, i+1)
            if text[i] == '('
                acc = v
            else
                acc = applyop(acc, text[i], v)
            end
            op = text[i]
            i = k
        end
        return acc, i+1
    end
end

function applyop(acc, op, v)
    if op == '+'
        acc + v
    elseif op == '*'
        acc * v
    else
        error("applyop")
    end        
end

function evaltoplevel(text)
    s = "(" * replace(text, " " => "") * ")"  # awkward, * not +?
    evalexpr(s, 1)[1]
end

filename = "/usr/local/google/home/xdavidliu/Documents/temp/input18.txt"
p1 = sum(evaltoplevel(s) for s in readlines(filename))
println("part 1 = ", p1)  # 12918250417632
