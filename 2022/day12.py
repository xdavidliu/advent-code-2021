import collections

def find_index(char, grid):
    out = []
    for r, row in enumerate(grid):
        for c, elem in enumerate(row):
            if elem == char:
                out.append((r, c))
    return out

def height(char):
    if char == 'S':
        return 0
    elif char == 'E':
        return 25
    else:
        return ord(char) - ord('a')

import math

def bfs(grid, start):
    nr = len(grid)
    nc = len(grid[0])
    q = collections.deque()
    q.append(start + (0,))
    dest = find_index('E', grid)[0]
    seen = set([start])
    dirs = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    while q:
        r, c, dist = q.popleft()
        for dr, dc in dirs:
            rr, cc = r + dr, c + dc
            if rr < 0 or rr >= nr or cc < 0 or cc >= nc:
                continue
            elif (rr, cc) in seen:
                continue
            else:
                v = grid[r][c]
                vv = grid[rr][cc]
                if height(vv) - height(v) > 1:
                    continue
                elif vv == 'E':
                    return dist + 1
                else:
                    q.append((rr, cc, dist+1))
                    seen.add((rr, cc))
    return math.inf

def solve():
    with open('data.txt', 'r') as file:
        grid = [ln.strip() for ln in file]
        dists = []
        dists.append(bfs(grid, find_index('S', grid)[0]))
        print(dists[0])
        for start in find_index('a', grid):
            dists.append(bfs(grid, start))
        print(min(dists))

solve()
