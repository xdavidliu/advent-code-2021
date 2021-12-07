def f1(d): return d
def f2(d): return d*(d+1)//2
def score(xs, a, f): return sum(f(abs(x-a)) for x in xs)

with open('data1.txt') as f:
    xs = [int(x) for x in f.read().strip().split(',')]
print(min(score(xs, a, f2) for a in range(min(xs), max(xs) + 1)))
