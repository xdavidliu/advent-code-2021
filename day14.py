with open('d.txt') as f:
    xs = f.readlines()

t = xs[0].strip()
d = {}
for l in xs[2:]:
    k, v = l.strip().split(' -> ')
    d[k] = v

from collections import Counter

c = dict()

def rec(s, i):
    r = c.get((s,i))
    if r is None:
        if i == 0:
            r = Counter(s)
        else:
            m = d[s]
            r = rec(s[0]+m, i-1) + rec(m+s[1], i-1)
            r[m] -= 1 
        c[(s,i)] = r
    return r

def solve(n):
    r = Counter()
    for a, b in zip(t, t[1:]):
        r += rec(a+b, n)
    r -= Counter(t)
    r[t[0]] += 1
    r[t[-1]] += 1

    m, x = min(r.values()), max(r.values())
    return x-m

print('part 1 =', solve(10))
print('part 2 =', solve(40))
