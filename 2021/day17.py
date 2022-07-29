xs = [102, 157]
ys = [-146, -90] 

import math

def hit(vx, vy):
   x, y = 0, 0
   m = 0
   while True:
       m = max(m, y)
       if xs[0] <= x <= xs[1] and ys[0] <= y <= ys[1]:
           return m
       if x > xs[1]: return -math.inf
       if y < ys[0] and vy <= 0: return -math.inf
       if x < xs[0] and vx == 0: return -math.inf
       x += vx
       y += vy
       if vx != 0: vx -= 1 
       vy -= 1

c = 0
m = -math.inf

# totally arbitrary; just trial and error
for vx in range(0, 1000):
    for vy in range(-500, 500):
        h = hit(vx, vy)
        if h == -math.inf: continue
        c += 1
        m = max(m, h)

print('part 1 =', m) 
print('part 2 =', c)
