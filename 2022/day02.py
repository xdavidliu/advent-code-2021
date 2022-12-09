num = dict(A=0, B=1, C=2, X=0, Y=1, Z=2)

def score(them, me):
    w = 0
    if them == me:
        w = 3
    elif (them + 1) % 3 == me:
        w = 6
    return me + 1 + w

def getme(them, what):
    if what == 1: return them
    elif what == 2: return (them + 1) % 3
    else: return (them + 2) % 3

with open('data.txt', 'r') as f:
    s1 = 0
    s2 = 0
    for ln in f:
        them = int(num[ln[0]])
        me = int(num[ln[2]])
        s1 += score(them, me)
        s2 += score(them, getme(them, me))
    print(s1)
    print(s2)
