gr = bytearray()
nc, nr = 0, 0

with open('d.txt') as f:
    for ln in f:
        row = ln.strip()
        if nc == 0: nc = len(row)
        nr += 1
        gr.extend(bytes(ln.strip(), 'utf'))

nn = nc * nr
assert len(gr) == nn

south = [None] * nn
east = [None] * nn

def RC(i): return i // nc, i % nc
def I(r, c): return r * nc + c
def Per(k, n): return 0 if k == n - 1 else k + 1

def Show():
    for r in range(nr):
        print(gr[r*nc:(r+1)*nc].decode('utf'))

for r in range(nr):
    for c in range(nc):
        i = I(r, c)
        i_s = I(Per(r, nr), c)
        i_e = I(r, Per(c, nc))
        south[i] = i_s
        east[i] = i_e

def TryToProgress(dir, symbol):
    swaps = []
    for i in range(nn):
        si = dir[i]
        if gr[i] == symbol and gr[si] == ord('.'):
            swaps.append((i, si))
    if not swaps: return False
    for x, y in swaps:
        gr[x], gr[y] = gr[y], gr[x]
    return True

def TryEast(): return TryToProgress(east, ord('>'))
def TrySouth(): return TryToProgress(south, ord('v'))

steps = 0
made_progress = True
while made_progress:
    made_progress = TryEast()
    # use this order to avoid short circuiting.
    made_progress = TrySouth() or made_progress
    steps += 1

print('part 1 =', steps)
# part 2 is free
