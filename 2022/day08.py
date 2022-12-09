with open('data.txt', 'r') as f:
    grid = [[int(c) for c in ln.strip()] for ln in f]

nr = len(grid)
nc = len(grid[0])

def foo():
    return [[False] * nc for _ in range(nr)]

b0, b1, b2, b3 = (foo() for _ in range(4))

import math

for r in range(nr):
    hi = -math.inf
    for c in range(nc):
        g = grid[r][c]
        if g > hi:
            hi = g
            b0[r][c] = True

for r in range(nr):
    hi = -math.inf
    for c in range(nc-1, -1, -1):
        g = grid[r][c]
        if g > hi:
            hi = g
            b1[r][c] = True

for c in range(nc):
    hi = -math.inf
    for r in range(nr):
        g = grid[r][c]
        if g > hi:
            hi = g
            b2[r][c] = True

for c in range(nc):
    hi = -math.inf
    for r in range(nr-1, -1, -1):
        g = grid[r][c]
        if g > hi:
            hi = g
            b3[r][c] = True

p1 = 0
for r in range(nr):
    for c in range(nc):
        p1 += b0[r][c] or b1[r][c] or b2[r][c] or b3[r][c]

print(p1)

# part 2

def bar():
    return [[0] * nc for _ in range(nr)]

d0, d1, d2, d3 = (bar() for _ in range(4))

for r in range(nr):
    old = grid[r][0]
    grid[r][0] = math.inf  # hack
    st = [0]
    d0[r][0] = 0
    for c in range(1, nc):
        g = grid[r][c]
        while grid[r][st[-1]] < g:
            st.pop()
        d0[r][c] = abs(c - st[-1])
        st.append(c)
    grid[r][0] = old

for r in range(nr):
    old = grid[r][nc-1]
    grid[r][nc-1] = math.inf
    st = [nc-1]
    d1[r][nc-1] = 0
    for c in range(nc-2, -1, -1):
        g = grid[r][c]
        while grid[r][st[-1]] < g:
            st.pop()
        d1[r][c] = abs(c - st[-1])
        st.append(c)
    grid[r][nc-1] = old

for c in range(nc):
    old = grid[0][c]
    grid[0][c] = math.inf  # hack
    st = [0]
    d2[0][c] = 0
    for r in range(1, nr):
        g = grid[r][c]
        while grid[st[-1]][c] < g:
            st.pop()
        d2[r][c] = abs(r - st[-1])
        st.append(r)
    grid[0][c] = old

for c in range(nc):
    old = grid[nr-1][c]
    grid[nr-1][c] = math.inf  # hack
    st = [nr-1]
    d3[nr-1][c] = 0
    for r in range(nr-2, -1, -1):
        g = grid[r][c]
        while grid[st[-1]][c] < g:
            st.pop()
        d3[r][c] = abs(r - st[-1])
        st.append(r)
    grid[nr-1][c] = old

best = 0
for r in range(nr):
    for c in range(nc):
        best = max(best, d0[r][c] * d1[r][c] * d2[r][c] * d3[r][c])

print(best)
