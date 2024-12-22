n = 9

indnext(i) = i == n ? 1 : i+1

function nextthree(a, i)
    i1 = indnext(i)
    i2 = indnext(i1)
    i3 = indnext(i2)
    i4 = indnext(i3)
    i4, a[[i1, i2, i3]]
end

function destination(x, nt)
    v = x-1
    while v in nt
        v -= 1
        if v == 0
            v = 9
        end
    end
    v
end

function move(a, i)
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
    nextind(i)
end

arr = [3,8,9,1,2,5,4,6,7]
i =  
move(arr, 1)
println(arr)
