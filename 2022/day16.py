import re
from collections import defaultdict, deque
from itertools import combinations
import math

def pair(v, w):
    return (v, w) if v < w else (w, v)

# TODO: this is messy as heck, need to go back and clean this all up
# but I finally got both parts. Need extensive babysitting and manual
# operation of this...

# 30 for part 1, 26 for part 2
# need to run separately due to overall messiness of this code
total_rem = 26

# see extensively messy notes at bottom
# also for part 1 of the test data, needed to do "heuristic skipping"
# but empirically this was not necessary for actual data nor part 2

pat = re.compile('Valve (.+) has flow rate=(.+); tunnels? leads? to valves? (.+)')
rate = dict()
dist = defaultdict(lambda: math.inf)
adj = dict()  # for finding starting points

start = 'AA'

with open('data.txt', 'r') as fl:
    for ln in fl:
        m = pat.match(ln.strip())
        v = m.group(1)
        rate[v] = int(m.group(2))
        neighbors = m.group(3).split(', ')
        for w in neighbors:
            dist[pair(v, w)] = 1
        adj[v] = neighbors

# perform bfs to get list of nonzero nodes reachable from start
# and going only through zero nodes
q = deque()
q.appendleft((start, 0))
seen = [start]
nonzero_starts = []
while q:
    x, d = q.pop()
    if rate[x] != 0:
        nonzero_starts.append((x, d))
    else:
        for y in adj[x]:
            if y not in seen:
                seen.append(y)
                q.appendleft((y, d+1))

def other(v, p):
    a, b = p
    return b if v == a else a

def remove(v, dist, os):
    for u, w in combinations(os, 2):
        uw = pair(u, w)
        uv = pair(u, v)
        wv = pair(w, v)
        # relies on defaultdict with math.inf default
        dist[uw] = min(dist[uw], dist[uv] + dist[wv])
    for o in os:
        del dist[pair(v, o)]    

# assume and confirm that all edges doubly linked
for v, es in adj.items():
    for x in es:
        assert v in adj[x]

for v, r in rate.items():
    if r != 0:
        continue
    os = [other(v,p) for p in dist if v in p]
    remove(v, dist, os)

best = 0
keyed_memo = dict()

def recurse(x, dist, score, rem, seq, memo):
    assert rem > 0
    assert rate[x] > 0
    global best
    global best_dbg
    score_next = score + (rem-1) * rate[x]
    seq_next = seq + (x,)
    # see notes below for when I got 643 instead of 943
    memo[seq_next] = max(memo[seq_next], score_next)
    if score_next > best:
        best = score_next
        print(best)

    os = [other(x,p) for p in dist if x in p]
    if not os:
        return
    dist_next = dist.copy()
    remove(x, dist_next, os)

    for o in os:
        rem_next = rem - 1 - dist[pair(o, x)]
        if rem_next > 1:
            recurse(o, dist_next, score_next, rem_next, seq_next, memo)
    # try skipping
    # edit never mind, interestingly the toy example in the problem
    # required skipping EE, but my real input did NOT require skipping.
    # That's lucky because when I turned on skipping, the code didn't
    # terminate...
    '''
    if rate[x] < 9:  # heuristic
        for o in os:
            rem_next = rem - dist[pair(o, x)]
            if rem_next > 1:
                recurse(o, dist, score, rem_next, seq, memo)
    '''

for s, d in nonzero_starts:
    rem = total_rem - d
    if rem > 0:
        keyed_memo[s] = defaultdict(lambda: -math.inf)
        recurse(s, dist, 0, total_rem-d, (), keyed_memo[s])

print(best)

def has_common(a, b):
    return any(x in b for x in a)



'''
best = -math.inf
best_pair = None

keyed_memo.keys()
# dict_keys(['MA', 'HR', 'DW', 'LF', 'MW'])

for k1, v1 in keyed_memo['DW'].items():
    for k2, v2 in keyed_memo['LF'].items():
        if not has_common(k1, k2):
            if v1 + v2 > best:
                best = v1 + v2
                best_pair = [k1, k2]

# So I manually did all the pairs up to the above, but the above hanged for some
# reason, so I just took the best one I found so far, which was 2536, and that was
# correct.

That's not the right answer; your answer is too high. If you're stuck, make sure you're using the full input data; there are also some general tips on the about page, or you can ask for hints on the subreddit. Please wait one minute before trying again. (You guessed 3334.)



from test:

ohh, it's because DD - HH is not accessible without skipping
more than 9
or also because the first time you get DD HH EE it gets 643 using a suboptimal skip
but then the second time you get it it doesn't beat best anymore

solution: always record in memo, even when it's not best

>>> keyed_memo['DD'][('DD', 'HH', 'EE')]
643  wtf????
>>> keyed_memo['JJ'][('JJ', 'BB', 'CC')]
764
>>> 643 + 764
1407
>>> rate['DD'] * (26-2) + rate['HH'] * (26-7) + rate['EE'] * (26-11)
943



========================
>>> best_pair
[('MA', 'II', 'AS', 'RU', 'PM', 'KQ', 'ED'), ('HR', 'DW', 'XO', 'VI', 'MW', 'FQ', 'TV')]
>>> keyed_memo['MA'][('MA', 'II', 'AS', 'RU', 'PM', 'KQ', 'ED')]
1857
>>> keyed_memo['HR'][('HR', 'DW', 'XO', 'VI', 'MW', 'FQ', 'TV')]
1477
>>> 1857 + 1477
3334

=======================

part 2 idea:
compute matrix of distances between rate > 0 nodes
wait but how the hell...

okay, how about compute permutations up to length 8
starting from one of the starting points, and length 8
from the other. That's 7! each = 5040.

Then find disjoint pairs (5000^2 = 25 million) and
just sum the scores

this idea is promising!!

wait no you can't the first one is NOT 7! but rather
7! times 15 choose 7, which is pretty large
but I guess it's not exactly factorial since the
first few are straightforward, and you can just be greedy

wait what about for each node, have a table of total_rem
with the largest possible score you can get given
k remaining seconds?

But for part 2, you would WANT it to be suboptimal
sometimes, so a single table might not work.

ALTERNATIVELY, just use the non-skipping ones from the beginning
except memoize it ... wait that doesn't work, since in order
to split in two, you NEED to skip some. Actually no because
the skipping we're talking about here is DELIBERATE skipping,
even if you don't do that, just by choosing a different o in os,
you WILL skip AFTERWARDS, so first try this idea.

Also try memoizing dist using the set of seen as the key.

okay I should probably do a recursive search except
recurse on TWO args instead of one. Start with the first two
accessible starting states.

'''
