import numpy as np
from itertools import permutations
from collections import deque

# TODO: precompute the 24 x 29 rotated ys so that you don't need to do the
# rotation inside match, which is called in a double loop.

rot = []
for x in [-1, 1]:
    for y in [-1, 1]:
        for z in [-1, 1]:
            for q in permutations([[x,0,0], [0,y,0], [0,0,z]]):
                m = np.array(q)
                if np.linalg.det(m) == 1:
                    rot.append(m)

def shift(y, z):
    sy = {tuple(x) for x in y}
    for a in y:
        for b in z:
            s = a-b
            ov = len(sy.intersection(tuple(x+s) for x in z))
            if ov >= 12: return s
    return None

def match(y, z):
    for r in rot:
        rz = [r.dot(x) for x in z]
        s = shift(y, rz)
        if s is None: continue
        else: return s, r
    return None

with open('d.txt') as f:
    xs = f.read().split('\n\n')

ys = []
for x in xs:
    ys.append([np.array(eval('[' + z.strip() + ']'))
               for z in x.split('\n')[1:]])

print('this will take about 5-10 minutes')

adj = [[None] * len(ys) for _ in ys]
for i in range(len(ys)):
    for k in range(len(ys)):
        print('double loop up to 29: ', i, k)
        if i >= k: continue
        m = match(ys[i], ys[k])
        if m is None: continue
        s, r = m
        adj[i][k] = m
        # Note in the opposite direction, these should be applied
        # in the OPPOSITE direction because rotations and trans do not
        # commute!
        adj[k][i] = -s, r.transpose()

found = {tuple(x) for x in ys[0]}
q = deque([0])
p = [None for _ in ys]
d = [np.array([0,0,0]) for _ in ys]
while q:
    i = q.popleft()
    for k in range(1, len(ys)):
        if p[k] is not None: continue
        if adj[i][k] is None: continue
        p[k] = i
        q.append(k)

for i in range(1, len(ys)):
    if p[i] is None: continue
    z = ys[i]
    i0 = i
    while p[i] is not None:
        s, r = adj[p[i]][i]
        if p[i] < i:
            z = [s + r.dot(v) for v in z]
            d[i0] = s + r.dot(d[i0])
        else:  # p[i] > i
            z = [r.dot(s + v) for v in z]
            d[i0] = r.dot(s + d[i0])
        i = p[i]
    found.update(tuple(v) for v in z)

def mdist(u, v):
    return sum(np.abs(u-v))

best = 0
for i in range(len(ys)):
    for k in range(len(ys)):
        if i >= k: continue
        best = max(best, mdist(d[i], d[k]))

print('part 1=', len(found))
print('part 2=', best)
