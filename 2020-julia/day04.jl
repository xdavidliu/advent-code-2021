function passport(block)
    re = r"(\w+):(.+)"
    pp = Dict{String, String}()
    for line in split(block, "\n")
        for word in split(line, " ")
            mch = match(re, word)
            pp[mch[1]] = mch[2]
        end
    end
    pp
end

function isvalid(pp)
    lf = length(pp)
    hcid = haskey(pp, "cid")
    lf == 7 && !hcid || lf == 8 && hcid
end

isfullyvalid(pp) = isvalid(pp) && all(check(k, w) for (k, w) in pp)

checkcid(word) = true

function checkint(word, pred)
    val = tryparse(Int, word)
    val != nothing && pred(val)
end

function between(low, hi)
    x -> low <= x <= hi
end

function checkhgt(word)
    endswith(word, "cm") && checkint(word[1:end-2], between(150, 193)) ||
    endswith(word, "in") && checkint(word[1:end-2], between(59, 76))
end

checkecl(word) = word in ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
checkpid(word) = match(r"^\d{9}$", word) != nothing
checkhcl(word) = match(r"^#[0-9a-f]{6}$", word) != nothing
checkbyr(word) = checkint(word, between(1920, 2002))
checkiyr(word) = checkint(word, between(2010, 2020))
checkeyr(word) = checkint(word, between(2020, 2030))
funcs = Dict([("byr", checkbyr), ("hcl", checkhcl), ("cid", checkcid), ("iyr", checkiyr), ("eyr", checkeyr), ("hgt", checkhgt), ("ecl", checkecl), ("pid", checkpid)])
check(key, word) = funcs[key](word)

text = read("/Users/xdavidliu/Documents/input04.txt", String)
blocks = split(text, "\n\n")
pps = [passport(b) for b in blocks]
println("part 1 = ", sum(isvalid(p) for p in pps))  # 190
println("part 2 = ", sum(isfullyvalid(p) for p in pps))  # 121
