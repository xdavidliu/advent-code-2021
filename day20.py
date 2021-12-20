from typing import List
from collections import Counter

todigit = {'.': '0', '#': '1'}
# when reading, replace all chars as above

Grid = List[str]  # 0 and 1

def Pad(gr: Grid, bg: str) -> Grid:
    p = []
    nj = len(gr[0])
    for _ in range(2):
        p.append(bg * (nj+4))
    for r in gr:
        p.append(bg+bg+r+bg+bg)
    for _ in range(2):
        p.append(bg * (nj+4))
    return p

def FromAlgo(gr: Grid, i, j, algo) -> str:
    cols = slice(j-1, j+2)
    s = ''.join(gr[k][cols] for k in range(i-1, i+2))
    return algo[int(s, 2)]

def FromBg(bg, algo):
    return algo[int(bg*9, 2)]

def Enhance(gr: Grid, algo: str) -> Grid:
    mi, mj = len(gr), len(gr[0])
    en = []
    for i in range(1, mi-1):
        r = []
        for j in range(1, mj-1):
            r.append(FromAlgo(gr, i, j, algo))
        en.append(''.join(r))
    return Pad(en, FromBg(gr[0][0], algo))

def Process(ln: str) -> str:
    return ln.strip().replace('.', '0').replace('#', '1')

with open('d.txt') as f:
    xs = f.readlines()

algo = Process(xs[0])
gr = [Process(ln) for ln in xs[2:]]
gr = Pad(gr, '0')
c = Counter()
for _ in range(50): gr = Enhance(gr, algo)
for r in gr: c.update(r)
print('part 2 =', c['1'])
print('replace 50 with 2 in code to get part 1.')
