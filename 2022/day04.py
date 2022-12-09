import re

with open('data.txt', 'r') as f:
    p1, p2 = 0, 0
    pat = re.compile('(\d+)-(\d+),(\d+)-(\d+)')
    for ln in f:
        m = pat.match(ln)
        a, b, c, d = map(int, m.groups())
        p1 += (a >= c and b <= d) or (a <= c and b >= d)
        p2 += not ((b < c) or (d < a))
    print(p1)
    print(p2)
