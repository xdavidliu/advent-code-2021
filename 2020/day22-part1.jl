# https://docs.julialang.org/en/v1/manual/types/#man-parametric-composite-types
# below based on 2024 swift util impl, based on CLRS

# surprised 2022 didn't have any BFS until day 22. Today's not even BFS either, just needs queue.
struct Queue{T}
    front::Vector{T}
    back::Vector{T}
end

import Base.isempty
import Base.push!
import Base.pop!

function isempty(q::Queue{T}) where {T}
    isempty(q.front) && isempty(q.back)
end

function push!(q::Queue{T}, x::T) where {T}
    push!(q.back, x)
end

function pop!(q::Queue{T}) where {T}
    if isempty(q.front)
        while length(q.back) > 1
            push!(q.front, pop!(q.back))
        end
        pop!(q.back)
    else
        pop!(q.front)
    end
end

function blocknums(bl)
    lines = split(bl, "\n")
    [parse(Int, x) for x in lines[2:end]]
end

function playonce!(q1, q2)
    x1, x2 = pop!(q1), pop!(q2)
    if x1 > x2
        push!(q1, x1)
        push!(q1, x2)
    elseif x1 < x2
        push!(q2, x2)
        push!(q2, x1)
    else
        error("playonce")
    end
end

function score!(q)
    a = Vector{Int}()
    while !isempty(q)
        push!(a, pop!(q))
    end
    reverse!(a)
    sum(i*x for (i, x) in enumerate(a))
end

function solve()
    filename = "/home/xdavidliu/Documents/input22.txt"
    text = read(filename, String)
    blocks = split(text, "\n\n")
    a1, a2 = blocknums(rstrip(blocks[1])), blocknums(rstrip(blocks[2]))
    reverse!(a1)
    reverse!(a2)
    q1 = Queue(a1, Vector{Int}())
    q2 = Queue(a2, Vector{Int}())
    i = 0
    while !isempty(q1) && !isempty(q2)
        playonce!(q1, q2)
        i += 1
    end
    win = isempty(q1) ? q2 : q1
    println("part 1 = ", score!(win))  # 32856
end

solve()
