from collections import namedtuple
from typing import Any, List, Union
from dataclasses import dataclass

@dataclass
class Pair:
    l: Any
    r: Any
    p: Any

@dataclass
class Leaf:
    v: int
    p: Pair

Node = Union[Pair, Leaf]

def Create(x) -> Node:
    if isinstance(x, int):
        return Leaf(v=x, p=None)
    else:  # isinstance(x, List)
        l, r = Create(x[0]), Create(x[1])
        p = Pair(l=l, r=r, p=None)
        l.p, r.p = p, p
        return p

def Copy(z: Node) -> Node:
    if isinstance(z, Leaf):
        return Leaf(v=z.v, p=None)
    else:
        p = Pair(l=Copy(z.l), r=Copy(z.r), p=None)
        p.l.p, p.r.p = p, p
        return p

def ToListOrInt(y: Node):
    if isinstance(y, Leaf):
        return y.v
    else:  # isinstance(y, Pair)
        return [ToListOrInt(y.l), ToListOrInt(y.r)]

def AddToFirst(z: Node, a: int) -> None:
    if isinstance(z, Leaf): z.v += a
    else: AddToFirst(z.l, a)

def AddToLast(z: Node, a: int) -> None:
    if isinstance(z, Leaf): z.v += a
    else: AddToLast(z.r, a)

def TryExplode(z: Pair) -> bool:
    f = FindExplode(z, 4)
    if f is None: return False
    if f.p is None:
        print('this shouldnt happen')
        print(ToListOrInt(f))
        print(ToListOrInt(z))
    if f is f.p.l:
        f.p.l = Leaf(v=0, p=f.p)
        AddToFirst(f.p.r, f.r.v)
        p, pp = f.p, f.p.p
        while pp and pp.l is p:
            p, pp = p.p, pp.p
        if pp:  # pp.r is p
            AddToLast(pp.l, f.l.v)
    else:  # f is f.p.r
        f.p.r = Leaf(v=0, p=f.p)
        AddToLast(f.p.l, f.l.v)
        p, pp = f.p, f.p.p
        while pp and pp.r is p:
            p, pp = p.p, pp.p
        if pp:  # pp.l is p
            AddToFirst(pp.r, f.r.v)
    return True

def FindExplode(z: Node, nest: int) -> Pair:
    if isinstance(z, Leaf): return None
    if nest == 0:
        if isinstance(z.l, Leaf) and isinstance(z.r, Leaf):
            return z
        else: return None
    return FindExplode(z.l, nest-1) or FindExplode(z.r, nest-1)

def Split(c: Leaf) -> bool:
    if c.v < 10: return False
    cl = Leaf(v=c.v//2, p=None)
    cr = Leaf(v=(1+c.v)//2, p=None)
    p = Pair(l=cl, r=cr, p=c.p)
    cl.p, cr.p = p, p
    if c.p.l is c:  c.p.l = p
    else: c.p.r = p
    return True

def TrySplit(z: Pair) -> bool:
    if isinstance(z.l, Leaf) and Split(z.l): return True
    elif isinstance(z.l, Pair) and TrySplit(z.l): return True
    elif isinstance(z.r, Leaf) and Split(z.r): return True
    elif isinstance(z.r, Pair): return TrySplit(z.r)
    else: return False

def Reduce(z: Pair) -> None:
    while True:
        if TryExplode(z):
            continue
        elif TrySplit(z):
            continue
        else: return

# Mutates x and y!
def Add(x: Node, y: Node) -> Node:
    z = Pair(l=x, r=y, p=None)
    x.p, y.p = z, z
    Reduce(z)
    return z

def Magnitude(x: Node) -> int:
    if isinstance(x, Leaf):
        return x.v
    else:  # isinstance(x, Pair)
        return 3 * Magnitude(x.l) + 2 * Magnitude(x.r)

with open('d.txt') as f:
    ps = [Create(eval(x)) for x in f.readlines()]

# Do part 2 first because this makes copies and doesn't mutate ps.
best = 0
for p in ps:
    for q in ps:
        if p is q: continue
        best = max(best, Magnitude(Add(Copy(p), Copy(q))))

s = ps[0]
for i in range(1, len(ps)):
    # Mutates ps.
    s = Add(s, ps[i])
print('part 1 =', Magnitude(s))
print('part 2 =', best)
