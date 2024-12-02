with open('d.txt') as f:
    xxs = f.readlines()

n = 10
xs = []

def plus(xs):
    for i in range(n):
        for j in range(n):
            xs[i][j] += 1

from collections import deque

def bfs(xs):
    q = deque()
    s = set()
    for i in range(n):
        for j in range(n):
            if xs[i][j] > 9:
                xs[i][j] = 0
                q.append((i,j))
                s.add((i,j))
    while q:
        i,j = q.popleft()
        for di, dj in [(-1, -1), (-1, 0), (-1, 1), (0, 1), (0, -1), (1, -1), (1, 0), (1, 1)]:
            ii = i + di
            jj = j + dj
            if not(0 <= ii < n and 0 <= jj < n):
                continue
            if (ii, jj) in s:
                continue
            xs[ii][jj] += 1
            if xs[ii][jj] > 9:
                xs[ii][jj] = 0
                q.append((ii, jj))
                s.add((ii, jj))
    return len(s)

for x in xxs:
    x = x.strip()
    xs.append([int(c) for c in x])

s = 0
found = False
for i in range(100):
    plus(xs)
    b = bfs(xs)
    if not found and b == n * n:
        found = True
        print('part 2 =', i + 1)
    s += b

i = 100
while True:
    plus(xs)
    b = bfs(xs)
    if not found and b == n * n:
        found = True
        print('part 2 =', i + 1)
        break
    s += b
    i += 1

print('part 1 =', s)
