with open('data.txt', 'r') as file:
    during = 0
    v = 1
    ans1 = 0
    crt = ['.'] * 240
    def update():
        global during
        global ans1
        during += 1
        pos = (during - 1) % 40
        if abs(pos - v) <= 1:
            crt[during - 1] = '#'
        if during % 40 == 20:
            ans1 += during * v
    for ln in file:
        ln = ln.strip()
        update()
        if ln == 'noop':
            pass
        else:  # addx 5
            update()
            v += int(ln.split()[-1])
    print(ans1)
    for k in range(0, 201, 40):
        print(''.join(crt[k:k+40]))
