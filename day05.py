import math

class Segment:
    def _XY(pstr):
        x, y = pstr.split(',')
        return int(x), int(y)

    def __init__(self, x1, y1, x2, y2):
        self._x1, self._y1 = x1, y1
        self._x2, self._y2 = x2, y2

    def __repr__(self):
        return f'Segment(x1={self._x1}, y1={self._y1}, x2={self._x2}, y2={self._y2})'


def ReadSegment(line):
    p, q = line.strip().split(' -> ')
    return Segment(*Segment._XY(p), *Segment._XY(q))
    

def ReadAll(filename):
    with open(filename) as f:
        return [ReadSegment(l) for l in f.readlines()]
    

class Grid:
    def __init__(self, size, include_diag):
        self._include_diag = include_diag
        self._data = [[0] * size for _ in range(size)]

    def _Delta(x1, x2):
        return 0 if x1 == x2 else int(math.copysign(1, x2 - x1))

    def Mark(self, seg):
        x, y = seg._x1, seg._y1
        dx = Grid._Delta(seg._x1, seg._x2)
        dy = Grid._Delta(seg._y1, seg._y2)
        if dx and dy and not self._include_diag:
            return
        self._data[x][y] += 1
        while (x, y) != (seg._x2, seg._y2):
            x += dx
            y += dy
            self._data[x][y] += 1

    def Score(self):
        return sum(1 for r in self._data for x in r if x > 1)


if __name__ == '__main__':
    segs = ReadAll('data.txt')
    include_diag = False
    grid = Grid(1000, include_diag)
    for s in segs:
        grid.Mark(s)
    print(grid.Score())
