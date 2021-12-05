from math import copysign

def ReadAll(filename):
    def Point(s):
        x, y = s.split(',')
        return int(x), int(y)
    def ReadSegment(line):
        p, q = line.strip().split(' -> ')
        return Point(p) + Point(q)
    with open(filename) as f:
        return [ReadSegment(l) for l in f.readlines()]

class Grid:
    def __init__(self, size, include_diag):
        self._include_diag = include_diag
        self._data = [[0] * size for _ in range(size)]

    def _Delta(x1, x2):
        return 0 if x1 == x2 else int(copysign(1, x2 - x1))

    def Mark(self, x, y, x2, y2):
        dx, dy = Grid._Delta(x, x2), Grid._Delta(y, y2)
        if dx and dy and not self._include_diag:
            return
        self._data[x][y] += 1
        while (x, y) != (x2, y2):
            x += dx
            y += dy
            self._data[x][y] += 1

    def Score(self):
        return sum(1 for r in self._data for x in r if x > 1)

if __name__ == '__main__':
    segs = ReadAll('data.txt')
    grid = Grid(1000, include_diag=True)
    for s in segs:
        grid.Mark(*s)
    print(grid.Score())
