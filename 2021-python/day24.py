# note: this program assumes some patterns particular to xdavidliu's input.
# may or may not be valid for other inputs

with open('d.txt') as f:
    INS = [a.strip() for a in f]

assert len(INS) == 14 * 18
G = [int(a[6:]) for a in INS[4::18]]
H = [int(a[6:]) for a in INS[5::18]]
P = [int(a[6:]) for a in INS[15::18]]

zero_conds = []

def Pretty(cond: tuple[int]):
    i, eq, s, d = cond
    eq = '=' if eq else '!='
    return f'[k{i} {eq} k{s} + {d}]'

# invariant: when calling Rec with i, z contains the terms of polynomial in zi.
def Rec(i, z: tuple[int], conds: tuple[tuple[int]]) -> None:
    if i == 14:  # recall zi is value of z BEFORE iteration i. Hence we want to complete iteration 13, so want z14, not z13!
        if not z and conds:  # found some conds that cause z = 0
            zero_conds.append(conds)
        return
    if not z:  # doesn't matter if is26 because zi / gi = 0 either way
        if not 1 <= H[i] <= 9:  # q = 0 guaranteed
            Rec(i+1, (i,), conds)
        else:
            eq = ((i, True, None, H[i]),)
            neq = ((i, False, None, H[i]),)
            Rec(i+1, (i,), conds + neq)
            Rec(i+1, (), conds + eq)
    else:  # z is non-empty
        is26 = G[i] == 26
        s = z[-1]
        d = P[s] + H[i]
        if abs(d) > 8:  # q = 0 guaranteed
            if G[i] == 26:  # (26, 0)
                Rec(i+1, z[:-1] + (i,), conds)
            else:  # (1, 0)
                Rec(i+1, z + (i,), conds)
        else:  # q = 1 possible
            eq = ((i, True, s, d),)
            neq = ((i, False, s, d),)
            if G[i] == 26:
                Rec(i+1, z[:-1] + (i,), conds + neq)  # (26, 0)
                Rec(i+1, z[:-1], conds + eq)  # (26, 1)   note NOT z[:-2]!!!
            else:  # g[i] == 1
                Rec(i+1, z + (i,), conds + neq)  # (1, 0)
                Rec(i+1, z, conds + eq)  # (1, 1)

Rec(0, (), ())

from operator import add, mul, floordiv, mod, eq

def Digits(k: int): return [int(c) for c in f'{k:014d}']

def Test(k: int):
    assert isinstance(k, int)
    k = Digits(k)
    assert 0 not in k  # from problem statement, digits cannot be 0
    var = dict(w=[0], x=[0], y=[0], z=[0])
    ops = dict(add=add, mul=mul, div=floordiv, eql=eq, mod=mod)
    i = 0
    for ins in INS:
        if ins.startswith('inp'):
            var[ins[4]][0] = k[i]
            i += 1
        else:
            op, v, b = ins.split()
            a = var[v][0]
            try:
                b = int(b)
            except ValueError:
                b = var[b][0]
            var[v][0] = int(ops[op](a, b))
    return var['z'][0]

def CheckCond(k: int, cond):
    k = Digits(k)
    for i, eq, j, d in cond:
        x = k[i] == k[j] + d
        if x != eq: return False
    return True

# interestingly there's only ONE set of conditions that gives z14 = 0
assert len(zero_conds) == 1

print('for part 1: These are the conditions for z = 0 at end:')
for c in zero_conds[0]:
    print(Pretty(c))
k = 92915979999498
kmin = 21611513911181
print('should be pretty easy to determine the extremal input satisfying this.')
print('if you want, can code up a topological sort, but can manually (ocularly?) in like 10 seconds.')
print(f'for xdavidliu, largest = {k}, smallest = {kmin}')

'''
xdavidliu's output conditions:
[k5 = k4 + 4]
[k7 = k6 + 2]
[k8 = k3 + 8]
[k10 = k9 + 0]
[k11 = k2 + -5]
[k12 = k1 + 7]
[k13 = k0 + -1]
'''

assert len(str(k)) == 14
assert len(str(kmin)) == 14
assert CheckCond(k, zero_conds[0])
assert CheckCond(kmin, zero_conds[0])
assert Test(k) == 0
assert Test(kmin) == 0

'''
Original attempt to do this problem by hand, which inspired the code above.
wi xi yi zi are values before i-th round. Rounds are i=0,1,2...13
ki are digits; k0 most significant, k13 least.

w0 x0 y0 z0 = 0

w = ki
x = zi % 26   xi doesn't matter
z = zi / gi             g = [1, 1, 1, 1, 1, 26, 1, 26, 26, 1, 26, 26, 26, 26]
x = zi % 26 + hi        h = [12, 11, 13, 11, 14, -10, 11, -9, -3, 13, -5, -10, -4, -5]

x = 1 if ki == zi % 26 + hi    ---> call this qi
    0 otherwise

x = 1 - qi

y = 25         yi doesn't matter
y = 25(1 - qi) 
y = 26 - 25 qi
z = zi / gi * (26 - 25 qi)

y = ki + pi          p = [4, 11, 5, 11, 14, 7, 11, 4, 6, 5, 9, 12, 14, 14]
y = (ki + pi) (1 - qi)

let si = zi % 26, i.e. least significant term in zi.

qi = int(ki == si + hi)

z[i+1] = zi / gi * (26 - 25 qi) + (ki + pi) (1 - qi)

let ti = ki + pi
note 1 <= ki <= 9 and 4 <= pi <= 14, so 5 <= ti <= 23.

gi = 1 or 26
qi = 0 or 1

(1, 0) -> z[i+1] = zi * 26 + ti
(1, 1) -> z[i+1] = zi
(26, 0) -> z[i+1] = zi / 26 * 26 + ti
                    i.e. zi with the smallest term ti instead of t[i-1]
(26, 1) -> z[i+1] = zi / 26 = z[i-1]  (that's TWO steps below!)

for i = 0,1,2,3,4 we have gi = 0
also hi > 10 so qi = 0

Hence, for i = 0, 1, 2, 3, 4; we have the (1, 0) case. Hence:
z0 = 0
z1 = [t0]
z2 = [t0, t1]
z3 = [t0, t1, t2]
z4 = [t0, t1, t2, t3]
z5 = [t0, t1, t2, t3, t4]

Next, g5 = 26;
s5 = t4; h5 = -10 -> q5 = int(k5 == t4 - 10 = k4 + 4)
Now it bifurcates. Consider the two cases:
case 0: k5 != k4 + 4 -> (26, 0) -> z6 = [t0, t1, t2, t3, t5]    # replaced t4 with t5 in z5
case 1: k5 = k4 + 4 -> (26, 1) -> z6 = z4 = [t0, t1, t2, t3]

g6 = 1; h6 = 11
case 0: s6 = t5 -> q6 = int(k6 == t5 + 11 = k5 + 18) = 0
  (1, 0) -> z7 = [t0, t1, t2, t3, t5, t6]  (missing t4)

case 1: q6 = int(k6 == t3 + 11 = k3 + 22) = 0
  (1, 0) -> z7 = [t0, t1, t2, t3, t6]

g7 = 26; s7 = t6 = k6 + 11; h7 = -9
q7 = int(k7 == k6 + 2).

case 00: k5 != k4 + 4 and k7 != k6 + 2
(26,0) -> z8 = [t0, t1, t2, t3, t5, t7]

case 01: k5 != k4 + 4 and k7 = k6 + 2
(26,1) -> z8 = z6 from case 0 = [t0, t1, t2, t3, t5]

case 10: k5 = k4 + 4 and k7 != k6 + 2
(26,0) -> z8 = [t0, t1, t2, t3, t7]

case 11: k5 = k4 + 4 and k7 = k6 + 2
(26,1) -> z8 = z6 from case 1 = z4 = [t0, t1, t2, t3]

g8 = 26, h8 = -3
for case 00 and 10, s8 = t7 = k7 + 4
q8 = int(k8 == k7 + 1)

case 000: k5 != k4 + 4 and k7 != k6 + 2 and k8 != k7 + 1
(26, 0) -> z9 = [t0, t1, t2, t3, t5, t8]

case 001: k5 != k4 + 4 and k7 != k6 + 2 and k8 == k7 + 1
(26, 1) -> z9 = z6 from case 0 = [t0, t1, t2, t3, t5]

case 010: k5 != k4 + 4 and k7 = k6 + 2 and k8 != k7 + 1
(26, 0) -> z9 = [t0, t1, t2, t3, t8]

case 11

need to get to z13

Wow, this is quickly getting out of hand. Need to write code for this...
'''
