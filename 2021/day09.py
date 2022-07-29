with open('data.txt') as f:
    ls = f.readlines()

ls = [x.strip() for x in ls]
n = len(ls[0])
ls = [[10] + list(map(int, x)) + [10] for x in ls]
tens = [[10] * (n+2)]
ls = tens + ls + tens

import collections

def basin(i, j, ls):
    f = set()
    d = collections.deque()
    d.append((i,j))
    f.add((i,j))
    while d:
        k, l = d.popleft()
        b = ls[k][l]
        for dk, dl in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            kk, ll = k+dk, l+dl
            if (kk, ll) in f: continue
            a = ls[kk][ll]
            if a >= 9 or a <= b: continue
            f.add((kk, ll))
            d.append((kk, ll))
    return len(f)

s = 0
bs = []
for j in range(1, n+1):
    for i in range(1, len(ls) - 1):
        x = ls[i][j] 
        if x < ls[i+1][j] and x < ls[i-1][j] and x < ls[i][j+1] and x < ls[i][j-1]:
            s += x + 1
            bs.append(basin(i, j, ls))

bs.sort(reverse=True)
print('part 1 = ', s)
print('part 2 = ', bs[0] * bs[1] * bs[2])
