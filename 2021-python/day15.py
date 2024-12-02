with open('d.txt') as f:
    xs = f.readlines()

ys = [list(map(int, x.strip())) for x in xs]

def five(ys):
    nj, ni = len(ys[0]), len(ys)
    zs = [[0] * (5*nj) for _ in range(5*ni)]
    for i in range(5*ni):
        for j in range(5*nj):
            d = i // ni + j // nj
            ii = i % ni
            jj = j % nj
            zs[i][j] = (ys[ii][jj] + d - 1) % 9 + 1
    return zs

import heapq
import itertools
import math

def solve(ys):
    nj, ni = len(ys[0]), len(ys)
    zs = [[math.inf] * nj for _ in range(ni)]
    zs[0][0] = 0
    q = [[0, 0, 0]]
    vis = [[False] * nj for _ in range(ni)]
    while q:
        x, i, j = heapq.heappop(q)
        if x > zs[i][j]: continue  # hack
        vis[i][j] = True
        if i == ni-1 and j == nj-1: return x
        for di, dj in [(-1, 0), (1, 0), (0, 1), (0, -1)]:
            ii, jj = i+di, j+dj
            if not (0 <= ii < ni): continue
            if not (0 <= jj < nj): continue
            if vis[ii][jj]: continue
            p = x + ys[ii][jj]
            if p < zs[ii][jj]:
                zs[ii][jj] = p
                heapq.heappush(q, [p, ii, jj])

print('part 1=', solve(ys))
print('part 2=', solve(five(ys)))
