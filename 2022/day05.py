# (remove the stack part of the input and manually
# type it here; only keep the instruction part in input)

from copy import deepcopy

st = [list('gdvzjsb'), list('zsmgvp'), list('clbswtqf'),
      list('hjgwmrvq'), list('clsnfmd'), list('rgcd'),
      list('hgtrjdsq'), list('pfv'), list('drstj')]

st2 = deepcopy(st)

with open('data.txt', 'r') as f:
    for ln in f:
        _, a, _, b, _, c = ln.split()
        a, b, c = map(int, [a,b,c])
        for _ in range(a):
            st[c-1].append(st[b-1].pop())
        st2[c-1].extend(st2[b-1][-a:])
        st2[b-1] = st2[b-1][:-a]
    for x in st:
        if x:
            print(x[-1], end='')
    print()
    for x in st2:
        if x:
            print(x[-1], end='')
    print()
