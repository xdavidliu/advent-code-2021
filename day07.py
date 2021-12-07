def score1(xs, a):
    return sum(abs(x - a) for x in xs)

def score2(xs, a):
    def f(k):
        return k*(k+1)//2
    return sum(f(abs(x-a)) for x in xs)

with open('data1.txt') as f:
    xs = [int(x) for x in f.read().strip().split(',')]
print(min(score2(xs, a) for a in range(min(xs), max(xs) + 1)))
