from itertools import chain
# source = 500, 0

def make_path(ln):
    return [tuple(map(int, s.split(','))) for s in ln.split(' -> ')]

def write_hash(grid, left, a, b):
    x0, y0 = a
    x1, y1 = b
    for x in range(min(x0, x1), 1 + max(x0, x1)):
        for y in range(min(y0, y1), 1 + max(y0, y1)):
            grid[y][x - left] = '#'

def solve(grid, left, right, bottom):
    count = 0
    while True:
        x, y = 500, 0
        if grid[y][x - left] == 'O':
            return count  # part 2
        while True:
            if y == bottom:
                return count
            elif grid[y + 1][x - left] == '.':
                y += 1
            elif x == left:
                return count
            elif grid[y + 1][x - left - 1] == '.':
                y += 1
                x -= 1
            elif x == right:
                return count
            elif grid[y + 1][x - left + 1] == '.':
                y += 1
                x += 1
            else:
                grid[y][x - left] = 'O'
                count += 1
                break

with open('data.txt', 'r') as file:
    paths = [make_path(ln.strip()) for ln in file]
    xs = list(x for x, _ in chain(*paths))
    left = min(xs)
    right = max(xs)
    bottom = max(y for _, y in chain(*paths))
    nrow = bottom + 1
    ncol = 1 + right - left
    grid = [['.'] * ncol for _ in range(nrow)]
    for path in paths:
        for i in range(1, len(path)):
            write_hash(grid, left, path[i-1], path[i])
    print(solve(grid, left, right, bottom))
    # part 2
    left = 500 - bottom - 2
    right = 500 + bottom + 2
    bottom += 2
    nrow = bottom + 1
    ncol = 1 + right - left
    grid = [['.'] * ncol for _ in range(nrow)]
    for path in paths:
        for i in range(1, len(path)):
            write_hash(grid, left, path[i-1], path[i])
    for x in range(ncol):
        grid[bottom][x] = '#'
    print(solve(grid, left, right, bottom))
