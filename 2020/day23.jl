function getf(a, n)  # f for "forward"
    f = Vector{Int}(undef, n)
    for i in eachindex(a)[1:end-1]
        f[a[i]] = a[i+1]
    end
    na = length(a)
    if n == na
        f[a[end]] = a[1]
    else
        f[a[end]] = na + 1
        for i in na+1:n-1
            f[i] = i+1
        end
        f[n] = a[1]
    end
    f
end

function iter!(f, i)
    i1 = f[i]
    i2 = f[i1]
    i3 = f[i2]
    i4 = f[i3]
    n = length(f)
    d = i - 1
    if d == 0
        d = n
    end
    while d == i1 || d == i2 || d == i3
        if d == 1
            d = n
        else
            d -= 1
        end
    end
    fd0 = f[d]
    f[d] = i1
    f[i3] = fd0
    f[i] = i4
    i4
end

function fromone(f)
    i = 1
    out = Vector{Int}()
    for _ in eachindex(f)[1:end-1]
        i = f[i]
        push!(out, i)
    end
    println("part 1 = ", join(out, ""))
end

# a = [3,8,9,1,2,5,4,6,7]
a = [9,5,2,3,1,6,4,8,7]

function part1()
    f = getf(a, 9)
    i = a[1]
    for _ in 1:100
        i = iter!(f, i)
    end
    fromone(f)
end

function part2()
    f = getf(a, 1_000_000)
    i = a[1]
    for _ in 1:10_000_000
        i = iter!(f, i)
    end
    i1 = f[1]
    i2 = f[i1]
    print("part 2 = ", i1 * i2)
end

part1()  # 25398647
part2()  # 363807398885
