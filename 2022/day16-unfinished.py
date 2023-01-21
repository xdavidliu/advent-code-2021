import re
import math
from collections import deque

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

param = 5  # play around with this to tradeoff running time with score
best = -math.inf

def recurse(valve, seen, score, time_left):
    global best
    others = nonzero_adjacent[valve]
    for other in others[:param]:
        dist = distances[(valve, other)]
        if dist + 1 >= time_left:
            best = max(best, score)
            return
        if other not in seen:
            new_time_left = time_left - dist - 1
            new_score = score + new_time_left * rate_of[other]
            recurse(other, seen + [valve], new_score, new_time_left)   


recurse('AA', [], 0, 30)
print(f'part 1 = {best}')
