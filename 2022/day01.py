def foo(s):
    return sum(map(int, s.split('\n')))

with open('data.txt', 'r') as f:
    s = f.read().strip().split('\n\n')  # for final \n
    r = [foo(x) for x in s]
    r.sort()
    print(f'part 1 = {r[-1]}')    
    print(f'part 2 = {r[-1] + r[-2] + r[-3]}')
