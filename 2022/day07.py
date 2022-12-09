from collections import defaultdict
from collections import Counter

total_sizes = dict()

def compute_total_size(dir, self_sizes, adj):
    memo = total_sizes.get(dir)
    if memo is not None:
        return memo
    sz = self_sizes[dir]
    for child in adj[dir]:
        sz += compute_total_size(child, self_sizes, adj)
    total_sizes[dir] = sz
    return sz

with open('data.txt', 'r') as f:
    pwd = ()
    self_sizes = Counter()
    adj = defaultdict(lambda: [])
    for ln in f:
        ln = ln.strip()
        if ln == '$ cd /':
            pwd = ('/',)
        elif ln == '$ cd ..':
            pwd = pwd[:-1]
        elif ln.startswith('$ cd '):
            pwd += (ln[5:],)
        elif ln.startswith('$ ls'):
            pass
        elif ln.startswith('dir'):
            adj[pwd].append(pwd + (ln[4:],))
        else:  # e.g. 64971 dhmprc.qpl
            sz = int(ln.split()[0])
            self_sizes[pwd] += sz
    compute_total_size(('/',), self_sizes, adj)
    print(sum(sz for sz in total_sizes.values() if sz <= 100000))
    diff = 30000000 - (70000000 - total_sizes[('/',)])
    print(min(sz for sz in total_sizes.values() if sz >= diff))
