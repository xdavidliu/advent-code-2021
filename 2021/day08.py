with open('data.txt') as f:
    ls = f.readlines()

s = 0
a = (2, 4, 3, 7)
for x in ls:
    ys = x[61:].strip().split()
    s += sum(1 for y in ys if len(y) in a)
print('part 1 = ', s)


'''
1  2
4  4
7  3
8  7

0  6   missing d which 4 has but 1 doesnt
6  6   missing c which both 4 and 1 has
9  6   missing e which neither 4 nor 1 has

2  5   is not a subset of 6
3  5   has all of 1
5  5   is a subset of 6

'''

known = {1: 2, 4: 4, 7: 3, 8: 7}
def bags(y):
    return [frozenset(x) for x in y.split()]

def key(bs):
    d = dict()
    s = [''] * 10
    for k, v in known.items():
        s[k] = next(b for b in bs if len(b) == v)
    def in1or4(x):
        return x <= s[1] or x <= s[4]
    def in1and4(x):
        return x <= s[1] and x <= s[4]
    bs6 = [b for b in bs if len(b) == 6]
    bs5 = [b for b in bs if len(b) == 5]
    s[9] = next(b for b in bs6 if not in1or4(s[8].difference(b)))
    s[6] = next(b for b in bs6 if in1and4(s[8].difference(b)))
    s[0] = next(b for b in bs6 if b not in (s[6], s[9]))
    s[3] = next(b for b in bs5 if s[1] < b)
    s[5] = next(b for b in bs5 if b < s[6])
    s[2] = next(b for b in bs5 if b not in (s[3], s[5]))
    return {x: i for i, x in enumerate(s)}

with open('data.txt') as f:
    ls = f.readlines()

s = 0
for x in ls:
    y, z = x.strip().split(' | ')
    k = key(bags(y))
    d = [k[b] for b in bags(z)]
    s += 1000 * d[0] + 100 * d[1] + 10 * d[2] + d[3]

print('part 2 = ', s)
