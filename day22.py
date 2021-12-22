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


def Parse(s):
    state, xyz = s.strip().split()
    t = []
    for c in xyz.split(','):
        t.append(tuple(map(int, c[2:].split('..'))))
    return state, tuple(t)


def Length(x): return x[1] - x[0] + 1


def Volume(g):
    x, y, z = g
    return Length(x) * Length(y) * Length(z)
    

def Solve(filename):
    with open(filename) as f:
        steps = [Parse(ln) for ln in f]
    state0, g0 = steps[0]
    assert state0 == 'on'
    dis = set([g0])
    for i in range(1, len(steps)):
        state, g = steps[i]
        print(f'solving step {i} of {len(steps)}; state = {state}, len(dis) = {len(dis)}')
        if state == 'on':
            q = [g]
            for d in dis:
                q = list(chain(*(Bite(k, d) for k in q)))
            dis.update(q)
        else:  # s == 'off
            rem = []
            add = []
            for d in dis:
                b = Bite(d, g)
                if b != [d]:
                    rem.append(d)
                    add.extend(b)
            dis.difference_update(rem)
            dis.update(add)
    return sum(Volume(d) for d in dis)

print('this will take about 1 hour')
print(Solve('d.txt'))
