with open('data.txt') as fl:
    xs = [int(line) for line in fl]

def count(xs):
    return sum(xs[i-1] < xs[i] for i in range(1, len(xs)))

print('part 1 = {}'.format(count(xs)))  # 1466

qs = [sum(xs[i:i+3]) for i in range(len(xs)-2)]

print('part 2 = {}'.format(count(qs)))  # 1491
