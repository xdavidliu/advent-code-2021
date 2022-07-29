def pair(s):
    x, y = s.strip().split(',')
    return int(x), int(y)

def fold(s):
    s = s.strip()
    isy = s[11] == 'y'
    i = int(s[13:])
    return isy,i

def image(x, i):
    return 2*x-i

with open('d.txt') as f:
    xs = f.readlines()
br = xs.index('\n')
ps = {pair(s) for s in xs[:br]}
fs = [fold(s) for s in xs[br+1:]]

def filt(ps, z, isy):
    qs = set()
    mn = 0
    for ix, iy in ps:
        im = 0
        i = iy if isy else ix
        if i < z:
            qs.add((ix, iy))
        else:
            im = image(z, i)
            mn = min(im, mn)
            qs.add((ix, im) if isy else (im, iy))
    if mn < 0:
        qs = {(ix, iy-mn) if isy else (ix-mn, iy) for ix, iy in qs}
    return qs

isy, z = fs[0]
ps = filt(ps, z, isy)
print('part 1 =', len(ps))

for isy, z in fs[1:]:
    ps = filt(ps, z, isy)

nx = 1 + max(x for x, _ in ps)
ny = 1 + max(y for _, y in ps)

g = [['.'] * nx for _ in range(ny)]
for ix, iy in ps:
    g[iy][ix] = '#'

print('part 2 below:')
for r in g:
    print(''.join(map(str,r)))
