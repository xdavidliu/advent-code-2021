def dist(x0, y0, x1, y1):
    return abs(x0 - x1) + abs(y0 - y1)

# closed on both ends, NOT python-style interval
def seg(y, xs, ys, xb, yb):
    d = dist(xs, ys, xb, yb)
    dd = d - abs(ys - y)
    if dd <= 0:
        return None  # empty
    return [xs-dd, xs+dd]

import re
pat = re.compile('Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)\n?')

def parse(ln):
    m = pat.match(ln)
    return tuple(map(int, [m.group(i) for i in range(1, 5)]))

# O(n^2); could be O(n log n) if we tried harder
def make_disjoint(segs):
    segs.sort(key=lambda s: (s[1], s[0]))
    for i in range(len(segs)):
        a = segs[i]
        if not a:
            continue
        for k in range(i+1, len(segs)):
            b = segs[k]
            if not b:
                continue
            if a[0] <= b[0] <= b[1] <= a[1]:
                b.clear()
            elif b[0] <= a[0] <= a[1] <= b[1]:
                a.clear()
                break  # to outer loop
            elif a[0] <= b[0]-1 <= a[1]:
                #a[1] = b[1]
                #b.clear()  # can't do this because breaks sort invariant
                b[0] = a[0]
                a.clear()
                break
            # other case cannot be true since we sorted above

def compute_disjoint_segs(tups, y):
    segs = [seg(y, *tup) for tup in tups]
    segs = [s for s in segs if s]  # remove Nones
    make_disjoint(segs)
    return [s for s in segs if s]  # remove empty

with open('data.txt', 'r') as file:
    y = 2000000
    #y = 10
    tups = [parse(ln) for ln in file]
    segs = compute_disjoint_segs(tups, y)
    bs = set((xb, yb) for _, _, xb, yb in tups)
    nby = sum(1 for xb, yb in bs if yb == y)
    print(-nby + sum(1+s[1]-s[0] for s in segs if s))
    # part 2
    for y in range(3000000, 4000005):
        segs = compute_disjoint_segs(tups, y)
        if len(segs) != 1:
            print(y, segs)
            # 3211051 [[-718699, 2960218], [2960220, 4308597]]
            # x = 2960219
            # >>> 4000000 * 2960219 + 3211051
# 11840879211051
