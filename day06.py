def update(c):
  prev0 = c[0]
  c[0] = c[1]
  c[1] = c[2]
  c[2] = c[3]
  c[3] = c[4]
  c[4] = c[5]
  c[5] = c[6]
  c[6] = prev0 + c[7]
  c[7] = c[8]
  c[8] = prev0
  
with open('data1.txt') as f:
  s = [int(x) for x in f.read().strip().split(',')]
  
from collections import Counter
c = [0] * 9
for k, v in Counter(s).items():
  c[k] = v
  
for _ in range(256):
  update(c)
  
print(sum(c))
