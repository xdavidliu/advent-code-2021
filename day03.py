with open('foo.txt') as f:
  lines = [l.strip() for l in f]

def filt(lines, pos, gr):
  part = {d: [l for l in lines if l[pos] == d] for d in '01'}
  if len(part['1']) >= len(part['0']):
    return part['1' if gr else '0']
  else:
    return part['0' if gr else '1']

def red(lines, gr):
  n = len(lines[0])
  i = 0
  while len(lines) > 1:
    lines = filt(lines, i, gr)
    i += 1
  return int(lines[0], 2)

print(red(lines, True) * red(lines, False))
