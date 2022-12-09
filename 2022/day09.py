dirx = dict(U=0, D=0, L=-1, R=1)
diry = dict(U=1, D=-1, L=0, R=0)

def hmove(hx, hy, dir):
    return hx + dirx[dir], hy + diry[dir]

def sign(x):
    if x == 0: return 0
    else: return abs(x) // x

def tmove(hx, hy, tx, ty):
    if abs(hx - tx) <= 1 and abs(hy - ty) <= 1:
        return tx, ty
    elif hx == tx:
        return tx, ty + sign(hy - ty)
    elif hy == ty:
        return tx + sign(hx - tx), ty
    else:  # diagonal
        return tx + sign(hx - tx), ty + sign(hy - ty)

with open('data.txt', 'r') as file:
    seen1, seen2 = set(), set()
    seen1.add((0,0))
    seen2.add((0,0))
    x, y = [0] * 10, [0] * 10
    for ln in file:
        dir, mag = ln.strip().split()
        for _ in range(int(mag)):
            x[0], y[0] = hmove(x[0], y[0], dir)
            #x[1], y[1] = tmove(x[0], y[0], x[1], y[1])
            for i in range(9):
                x[i+1], y[i+1] = tmove(x[i], y[i], x[i+1], y[i+1])
            seen1.add((x[1], y[1]))
            seen2.add((x[-1], y[-1]))
    print(len(seen1))
    print(len(seen2))
