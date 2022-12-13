import functools

def compare(left, right):
    if isinstance(left, int) and isinstance(right, int):
        return left - right
    elif isinstance(left, int):
        return compare([left], right)
    elif isinstance(right, int):
        return compare(left, [right])
    else:
        i = 0
        while i < len(left) and i < len(right):
            res = compare(left[i], right[i])
            if res != 0:
                return res
            i += 1
        return len(left) - len(right)

with open('data.txt', 'r') as file:
    paragraphs = file.read().split('\n\n')
    pairs = [list(map(eval, p.strip().split('\n'))) for p in paragraphs]
    p1 = 0
    for i, (left, right) in enumerate(pairs):
        if compare(left, right) < 0:
            p1 += i + 1
    print(p1)
    # part 2
    items = []
    items.append([[2]])
    items.append([[6]])
    for (left, right) in pairs:
        items.append(left)
        items.append(right)
    items.sort(key=functools.cmp_to_key(compare))
    print((1 + items.index([[2]])) * (1 + items.index([[6]])))
