from fractions import Fraction
from typing import List
from numbers import Real

def parse_nums(line: str):
    toks = [word for word in line.split() if word != '@']
    return [int(tok[:-1] if tok.endswith(',') else tok) for tok in toks]

def sign(x: Real) -> int:
    if x > 0: return 1
    elif x < 0: return -1
    else: return 0

def has_intersect(p1: List[str], p2: List[str], lo: int, hi: int) -> bool:
    x1, y1, _, vx1, vy1, _ = p1
    x2, y2, _, vx2, vy2, _ = p2
    m1, m2 = Fraction(vy1, vx1), Fraction(vy2, vx2)
    if m1 == m2: return False
    b1, b2 = y1 - m1 * x1, y2 - m2 * x2
    x3 = (b2 - b1) / (m1 - m2)  # Fraction div works fine
    if not lo <= x3 <= hi: return False
    if sign(vx1) != sign(x3 - x1): return False
    if sign(vx2) != sign(x3 - x2): return False
    y3 = m1 * x3 + b1
    if not lo <= y3 <= hi: return False
    if sign(vy1) != sign(y3 - y1): return False
    if sign(vy2) != sign(y3 - y2): return False
    return True

def foo():
    lo, hi = 200000000000000, 400000000000000
    with open('data.txt') as file:
        rows = [parse_nums(line) for line in file]
    part1 = 0
    for i in range(len(rows)-1):
        for k in range(1+i, len(rows)):
            part1 += has_intersect(rows[i], rows[k], lo, hi)
    print('part 1 = {}'.format(part1))  # 16172

if __name__ == '__main__':
    foo()
