horiz = 0
depth = 0
aim, d2 = 0, 0

def move(dir, steps):
    global horiz, depth, d2, aim
    if dir == 'forward':
        horiz += steps
        d2 += aim * steps
    elif dir == 'down':
        depth += steps
        aim += steps
    elif dir == 'up':
        depth -= steps
        aim -= steps

def parse(line):
    dir, s = line.split()
    return dir, int(s)

with open('data.txt') as fl:
    for line in fl:
        move(*parse(line))

print('part 1 = {}'.format(horiz * depth))  # 2039912
print('part 2 = {}'.format(horiz * d2))  # 1942068080
