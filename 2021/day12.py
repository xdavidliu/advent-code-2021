from collections import defaultdict

with open('d.txt') as f:
    xs = f.readlines()

adj = defaultdict(set)

for x in xs:
    a, b = x.strip().split('-')
    adj[a].add(b)
    adj[b].add(a)

def visit(g, spent):
    p = g[-1]
    if p == 'end':
        return 1
    s = 0
    for x in adj[p]:
        if x.isupper() or x not in g:
            s += visit(g + [x], spent)
        elif 1 == g.count(x) and (not spent) and x != 'start':
            s += visit(g + [x], True)
    return s

print('part 1 =', visit(['start'], True))
print('part 2 =', visit(['start'], False))
