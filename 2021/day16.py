with open('d.txt') as f:
    s = f.read().strip()

def mybin(c):
    return format(int(c, 16), '04b')

def binstr(s):
    k = ''.join(map(mybin, s))
    # assume not all zero
    i = k[::-1].index('1')
    # return k[:len(k)-i]
    return k

from operator import mul
from functools import reduce

def consume(s, i, v):
    b = binstr(s)
    dv = int(b[i:i+3], 2)
    v[0] += dv
    t = int(b[i+3:i+6], 2)
    if t == 4:  # literal
        k = i+6
        ba = ''
        while b[k] == '1':
            ba += b[k+1:k+5]
            k += 5
        ba += b[k+1:k+5]
        return int(ba, 2), k + 5
    opers = []
    if b[i+6] == '0':
        n = int(b[i+7:i+7+15], 2)
        k = i+7+15
        while k < i+7+15+n:
            a, k = consume(s, k, v)
            opers.append(a)
        k = i+7+15+n
    elif b[i+6] == '1':
        nsub = int(b[i+7:i+7+11], 2)
        k = i+7+11
        for _ in range(nsub):
            a, k = consume(s, k, v)
            opers.append(a)
    if t == 0:
        aa = sum(opers)
    elif t == 1:
        aa = reduce(mul, opers)
    elif t == 2:
        aa = min(opers)
    elif t == 3:
        aa = max(opers)
    elif t == 5:
        aa = int(opers[0] > opers[1])
    elif t == 6:
        aa = int(opers[0] < opers[1])
    elif t == 7:
        aa = int(opers[0] == opers[1])
    return aa, k

v = [0]
a, _ = consume(s, 0, v)
# a, _ = consume('04005AC33890', 0, v)
print('part1 =', v[0])
print('part2 =', a)
