function applymask(mask, val, zeroelem, oneelem)
    for i in 1:length(mask)
        ch = mask[end-i+1]
        if ch == oneelem
            val |= 1 << (i-1)
        elseif ch == zeroelem
            val &= ~(1 << (i-1))
        end
    end
    val
end

function addrmask(mask, addr, val, memtwo)
    xlocs = [i for (i, v) in enumerate(mask) if v == 'X']
    l = length(xlocs)
    maskarr = repeat([0], length(mask))
    for i in eachindex(mask)
        if mask[i] == '1'
            maskarr[i] = 1
        elseif mask[i] == '0'
            sh = length(mask) - i
            maskarr[i] = (addr >> sh) & 1
        end  # ignore the 'X' case
    end
    for bits in 0:2^l-1
        for k in eachindex(xlocs)
            maskarr[xlocs[k]] = (bits >> (k-1)) & 1
        end
        maskedaddr = applymask(maskarr, addr, 0, 1)
        memtwo[maskedaddr] = val
    end
end

mem = Dict{Int, Int}()
memtwo = Dict{Int, Int}()
lines = readlines("/home/xdavidliu/Documents/aoc/input14.txt")
prefix = "mask = "
memre = r"^mem\[(\d+)\] = (\d+)$"
mask = ""
for line in lines
    if startswith(line, prefix)
        mask = line[1+length(prefix):end]
    else
        mch = match(memre, line)
        addr = parse(Int, mch[1])
        val = parse(Int, mch[2])
        mem[addr] = applymask(mask, val, '0', '1')
        addrmask(mask, addr, val, memtwo)
    end
end
p1 = sum(v for (_, v) in mem)
println("part 1 = ", p1)  # 7817357407588
p2 = sum(v for (_, v) in memtwo)
println("part 2 = ", p2)  # 4335927555692
