import re
import math
from collections import deque, defaultdict
from itertools import combinations

pat = re.compile(r'Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? ([ \w,]+)\n')

adjacent = dict()
nonzero_valves = list()
rate_of = dict()

with open('data1.txt', 'r') as file:
    for line in file:
        mat = pat.fullmatch(line)
        valve = mat.group(1)
        rate = int(mat.group(2))
        if rate != 0:
            nonzero_valves.append(valve)
            rate_of[valve] = rate
        adjacent[valve] = mat.group(3).split(', ')  # neighbors

def shortest_dist(origin, destination):
    if origin == destination:
        return 0
    queue = deque()
    queue.appendleft((origin, 0))
    seen = set()
    seen.add(origin)
    while queue:
        valve, dist = queue.pop()
        for nb in adjacent[valve]:
            if nb == destination:
                return dist + 1
            if nb not in seen:
                seen.add(nb)
                queue.appendleft((nb, dist + 1))
    return math.inf

distances = dict()
start = 'AA'

# seems to be true for my data
assert start not in nonzero_valves

nonzero_valves.sort(key=lambda v: shortest_dist(start, v))

for origin in nonzero_valves:
    distances[(start, origin)] = shortest_dist(start, origin)
    for destination in nonzero_valves:
        distances[(origin, destination)] = shortest_dist(origin, destination)

# TODO: rename this variable, since it includes start
nonzero_adjacent = dict()
for valve in nonzero_valves + ['AA']:
    others = [nb for nb in nonzero_valves if valve != nb]
    others.sort(key = lambda nb: distances[(valve, nb)])
    nonzero_adjacent[valve] = others

param = 5  # play around with this to trade off running time with score
best = -math.inf
record = defaultdict(list)

def recurse(valve, seen, score, time_left):
    global best
    others = nonzero_adjacent[valve]
    if score > 0:
        record[seen[0]].append((score, seen))
    best = max(best, score)
    for other in others[:param]:
        if other in seen:
            continue
        dist = distances[(valve, other)]
        if dist + 1 < time_left:
            new_time_left = time_left - dist - 1
            new_score = score + new_time_left * rate_of[other]
            recurse(other, seen + (other,), new_score, new_time_left)   

recurse('AA', (), 0, 30)
print(f'part 1 = {best}')

param = 5
# don't need to reset best
record = defaultdict(list)
other_best = -math.inf

recurse('AA', (), 0, 26)

for tups in record.values():
    # score is first element, reverse so high scores first
    tups.sort(reverse=True)

for r1, r2 in combinations(record.values(), 2):
    for score1, seen1 in r1:
        for score2, seen2 in r2:
            if score1 + score2 <= other_best:
                break  # because sorted, so next score2 is only worse
            if not any(x in seen2 for x in seen1):
                other_best = score1 + score2

print(f'part 2 = {other_best}')
