from itertools import chain

def Bite(u, b):
    """Returns list of disjoint rectangles from u after biting with b."""
    # print(f'biting u={u}, b={b}')
    if not b: return []
    u00, b00 = u[0][0], b[0][0]
    u01, b01 = u[0][1], b[0][1]
    g = (max(u00, b00), min(u01, b01))
    if g[0] > g[1]: return [u]  # No effect.
    out = []
    if u00 < b00:
        out.append(((u00, b00-1),) + u[1:])
    if u01 > b01:
        out.append(((b01+1, u01),) + u[1:])
    for p in Bite(u[1:], b[1:]):
        out.append((g,) + p)
    return out


def Constrain(r):
    if r[0] > 50: return None
    if r[1] < -50: return None
    t = list(r)
    if r[0] < -50: t[0] = -50
    if r[1] > 50: t[1] = 50
    return tuple(t)


def Parse(s, constrain):
    state, xyz = s.strip().split()
    t = []
    for c in xyz.split(','):
        r = tuple(map(int, c[2:].split('..')))
        if constrain:
            r = Constrain(r)
            if r is None: return None
        t.append(r)
    return state, tuple(t)


def Length(x): return x[1] - x[0] + 1


def Volume(g):
    x, y, z = g
    return Length(x) * Length(y) * Length(z)
    

def GetSteps(filename, constrain):
    with open(filename) as f:
        steps = [Parse(ln, constrain) for ln in f]
    return [s for s in steps if s is not None]


def Solve(steps):
    state0, g0 = steps[0]
    assert state0 == 'on'
    dis = [g0]
    for i in range(1, len(steps)):
        state, g = steps[i]
        dis = list(chain(*(Bite(d, g) for d in dis)))
        if state == 'on':
            dis.append(g)
    return sum(Volume(d) for d in dis)

f = 'd.txt'
print('part 1 =', Solve(GetSteps(f, True)))
print('solving for part 2; this will take about 20 seconds.')
print('part 2 =', Solve(GetSteps(f, False)))
