def score(c):
    if c.isupper():
        return 27 + ord(c) - ord('A')
    else:
        return 1 + ord(c) - ord('a')

def half(ln):
    n = len(ln)
    a = ln[:n//2]
    b = ln[n//2:]
    x = list(set(a).intersection(b))[0]
    return score(x)

with open('data.txt', 'r') as f:
    s1, s2 = 0, 0
    fl = [ln for ln in f]
    for ln in fl:
        s1 += half(ln)
    for i in range(len(fl)//3):
        a = fl[3*i].strip()  # strip newline
        b = fl[3*i+1].strip()
        c = fl[3*i+2].strip()
        x = list(set(a).intersection(b).intersection(c))[0]
        s2 += score(x)
    print(s1)
    print(s2)
