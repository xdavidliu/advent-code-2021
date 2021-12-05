class Board:

  def _ParseGrid(text):
    lines = text.split('\n')
    return [[int(x) for x in l.split()] for l in lines]

  def _InitMap(grid):
    return {grid[i][j]: (i,j) for i in range(5) for j in range(5)}

  def _ComputeScore(self, draw):
    if self._score is not None:
      return
    s = 0
    for k, v in self._map.items():
      i, j = v
      if not self._found[i][j]:
        s += k 
    self._score = draw * s

  def __init__(self, text):
    self._rc = [0] * 10
    self._score = None
    self._found = [[False] * 5 for _ in range(5)]
    grid = Board._ParseGrid(text) 
    self._map = Board._InitMap(grid)

  def _incr(self, i, draw):
    self._rc[i] += 1
    if self._rc[i] == 5:
      self._ComputeScore(draw)

  def Score(self):
    return self._score

  def Draw(self, num):
    g = self._map.get(num)
    if not g:
      return
    i, j = g 
    self._found[i][j] = True
    self._incr(i, num)
    self._incr(j + 5, num)


def ReadProblem(filename):
  with open(filename) as f:
    r = f.read()
  xs = r.split('\n\n')
  draws = [int(y) for y in xs[0].split(',')]
  boards = [Board(x) for x in xs[1:]]
  return draws, boards


def Solve():
  draws, boards = ReadProblem('p.txt')
  for num in draws:
    for b in boards:
      if b.Score() is None:
        b.Draw(num)
        if b.Score() is not None:
          print(b.Score())


if __name__ == '__main__':  
  Solve()
  # part 1 is first line printed, part 2 is last line.
