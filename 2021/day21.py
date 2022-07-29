from itertools import cycle
from typing import List
import numpy as np
from collections import Counter

def solve1(x):
    d = cycle(range(1, 101))
    x = list(x)
    s = [0, 0]
    t, j = 0, 0
    while True:
        x[j] += next(d) + next(d) + next(d)
        t += 3
        x[j] = (x[j] - 1) % 10 + 1
        s[j] += x[j]
        if s[j] >= 1000: return s[1-j] * t
        j = 1 - j

def solve2(x):
    x = list(x)
    z = (1,2,3)
    cc = Counter(a+b+c for a in z for b in z for c in z)
    c = [0] * 10
    for k,v in cc.items():
        c[k] = v
    dp = np.zeros((11,11,21,21), np.int64)
    ddp = np.zeros((11,11,21,21), np.int64)
    dp[x[0]][x[1]][0][0] = 1
    t = 0
    u = [0, 0]
    nz = True
    while nz:
        nz = False
        for x1 in range(1,10+1):
            for x2 in range(1,10+1):
                for s1 in range(0,20+1):
                    for s2 in range(0,20+1):
                        a = dp[x1][x2][s1][s2]
                        if a == 0: continue
                        nz = True
                        xa = [x1, x2]
                        sa = [s1, s2]
                        for k in range(3,9+1):
                            xx = (xa[t] + k - 1) % 10 + 1
                            ss = sa[t] + xx
                            dd = a * c[k]
                            if ss >= 21:
                                u[t] += dd
                            elif t == 0:
                                ddp[xx][x2][ss][s2] += dd
                            else:  # t == 1
                                ddp[x1][xx][s1][ss] += dd
        dp, ddp, = ddp, dp
        ddp.fill(0)
        t = 1 - t
    return max(u)

x = (8,4)
print('part 1 =', solve1(x))
print('part 2 =', solve2(x))
