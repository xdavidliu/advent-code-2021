n = 9

indnext(i) = i == n ? 1 : i+1
valprev(x) = x == 1 ? 9 : x-1

function nextthree(a, i)
    i1 = indnext(i)
    i2 = indnext(i1)
    i3 = indnext(i2)
    i4 = indnext(i3)
    i4, a[[i1, i2, i3]]
end

function destination(x, nt)
    v = valprev(x)
    while v in nt
        v = valprev(v)
    end
    v
end

function move!(a, i)
    r, nt = nextthree(a, i)
    l = indnext(i)
    dest = destination(a[i], nt)
    while true
        x = a[l] = a[r]
        l = indnext(l)
        r = indnext(r)
        if x == dest
            break
        end
    end
    for x in nt
        a[l] = x
        l = indnext(l)
    end
end

function collect(a)
    i = 1
    while a[i] != 1
        i = indnext(i)
    end
    out = Vector{Int}()
    for _ in 1:8
        i = indnext(i)
        push!(out, a[i])
    end
    out
end

# arr = [3,8,9,1,2,5,4,6,7]
arr = [9,5,2,3,1,6,4,8,7]
i = 1
for _ in 1:100
    move!(arr, i)
    i = indnext(i)
end
println("part 1 = ", join(collect(arr), ""))  # 25398647
