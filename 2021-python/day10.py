with open('d.txt') as f:
    xs = f.readlines()

m = {')': '(', ']': '[', '}': '{', '>': '<'}
q = {')': 3, ']': 57, '}': 1197, '>': 25137}
qq = {'(': 1, '[': 2, '{': 3, '<': 4}

def unwind(t):
    s = 0
    while t:
        s *= 5
        s += qq[t.pop()]
    return s

def score(x):
    t = []
    for c in x:
        y = m.get(c)
        if y:
            if not t:
                return (q[c], None)
            elif t[-1] == y:
                t.pop()
            else:
                return (q[c], None)
        else:
            t.append(c)
    return (None, unwind(t))

s = 0
u = []
for x in xs:
    x = x.strip()
    g, h = score(x)
    if h is None:
        s += g
    else:  # g is None
        u.append(h)

print('part 1 =', s)
u.sort()
print('part 2 =', u[len(u)//2])
