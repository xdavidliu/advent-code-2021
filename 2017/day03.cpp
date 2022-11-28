#include <iostream>
#include <cmath>
#include <vector>

int corner(int n) {
    int i = 1;
    while (n >= (i+2) * (i+2)) { i += 2; }
    return i;
}

int dist(int x, int y) {
    return std::abs(x) + std::abs(y);
}

int solve1(int input) {
    int c = corner(input);
    int v = c * c;
    if (v == input) {
        int x = (c - 1) / 2;
        return dist(x, x);
    }
    int x = (c + 1) / 2;
    int y = -x;
    if (input <= v + c + 1) {
        return dist(x, y + input - v);
    }
    y += c + 1;
    v += c + 1;
    if (input <= v + c + 1) {
        return dist(x - input + v, y);
    }
    x -= c + 1;
    v += c + 1;
    if (input <= v + c + 1) {
        return dist(x, y - input + v);
    }
    y -= c + 1;
    v += c + 1;
    if (input <= v + c + 1) {
        return dist(x + input - v, y);
    }
    return -1;
}

int solve2(int input) {
    std::vector<int> cur{1, 2, 4, 5, 10, 11, 23, 25};
    cur.resize(input);
    std::vector<int> prev(input);
    int prev_sz = 0;
    while (cur[prev_sz + 8 - 1] <= input) {
        cur.swap(prev);
        prev_sz += 8;
        cur[0] = prev[prev_sz-1] + prev[0];
        cur[1] = 2 * cur[0] + prev[1];
        const int cur_quarter = (8 + prev_sz) / 4;
        int i;
        for (i = 2; i < cur_quarter - 2; ++i) {
            cur[i] = cur[i-1] + prev[i] + prev[i-1] + prev[i-2];
        }
        // i = cur_quarter - 2
        cur[i] = cur[i-1] + prev[i-1] + prev[i-2];
        ++i;  // i = cur_quarter - 1, last in east segment
        cur[i] = cur[i-1] + prev[i-2];
        ++i;  // i = cur_quarter, first elem in north segment
        cur[i] = cur[i-1] + cur[i-2] + prev[i-2] + prev[i-3];
        ++i;  // i = 1 + cur_quarter, second elem in north segment
        for (; i < 2 * cur_quarter - 2; ++i) {
            cur[i] = cur[i-1] + prev[i-2] + prev[i-3] + prev[i-4];
        }
        // i = 2 * cur_quarter - 2
        cur[i] = cur[i-1] + prev[i-3] + prev[i-4];
        ++i;  // i = 2 * cur_quarter - 1
        cur[i] = cur[i-1] + prev[i-4];

        ++i;  // i = 2 * cur_quarter, first elem in west segment
        cur[i] = cur[i-1] + cur[i-2] + prev[i-4] + prev[i-5];
        ++i;  // i = 2 * cur_quarter + 1
        for (; i < 3 * cur_quarter - 2; ++i) {
            cur[i] = cur[i-1] + prev[i-4] + prev[i-5] + prev[i-6];
        }
        // i = 3 * cur_quarter - 2
        cur[i] = cur[i-1] + prev[i-5] + prev[i-6];
        ++i;  // i = 3 * cur_quarter - 1
        cur[i] = cur[i-1] + prev[i-6];
        ++i;  // i = 3 * cur_quarter
        cur[i] = cur[i-1] + cur[i-2] + prev[i-6] + prev[i-7];

        ++i;  // i = 3 * cur_quarter + 1
        for (; i < 4 * cur_quarter - 2; ++i) {
            cur[i] = cur[i-1] + prev[i-6] + prev[i-7] + prev[i-8];
        }
        // i = 4 * cur_quarter - 2
        cur[i] = cur[i-1] + prev[i-7] + prev[i-8] + cur[0];
        ++i;  // i = 4 * cur_quarter - 1
        cur[i] = cur[i-1] + prev[i-8] + cur[0];
    }
    /*
    for (int i = 0; i < prev_sz + 8; ++i) {
        std::cout << cur[i] << ' ';
    }
    std::cout << '\n';
    */
    for (int i = 0; i < prev_sz + 8; ++i) {
        if (cur[i] > input) {
            return cur[i];
        }
    }
    return -1;  // error
}

int main(void) {
    int input = 277678;
    std::cout << "part 1 = " << solve1(input) << '\n';
    std::cout << "part 2 = " << solve2(input) << '\n';
}

/*
 * nov 18, 2022

https://adventofcode.com/2017/day/3#part2


147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806  880  931

inner layer 1 elem
next one is 8
next 16

each one 8 more

(can count with four "spiral")

from above, let's try to do the next layer

 0   1   2   3
26  54  57  59

 4   5   6   7
122 133 142 147

 8    9    10   11
304  330  351  362

 12  13   14    15
747  806  880  931


next one is gonna be 16 + 8 = 24 (elem in next layer), so indices from 0 to 23

let p be prev and c be cur

c0 would be 931 + 26, which is p15 + p0
c1 is 931 + 26 + 54 + c0, which is p15 + p0 + p1 + c0
  = 2 p15 + 2 p0 + p1 = 2 c0 + p1

these are the bulk ones
c2 is 26 + 54 + 57 + c1, which is p0 + p1 + p2 + c1

c3 = p1 + p2 + p3 + c2

c4 = p2 + p3 + c3
c5 = p3 + c4

pretty easy, now for the north side. Copy and paste the grid here for easier reference

                     c6
147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806  880  931

c6 = p3 + p4 + c5 + c4   // note two c's

ordinary bulk
c7 = p3 + p4 + p5 + c6
c8 = p4 + p5 + p6 + c7
c9 = p5 + p6 + p7 + c8

c10 = p6 + p7 + c9
c11 = p7 + c10

hmm, end is pretty simple. Next do west edge.


c12  147  142  133  122   59
     304    5    4    2   57
     330   10    1    1   54
     351   11   23   25   26
     362  747  806  880  931

c12 = p7 + p8 + c11 + c10

ordinary bulk ones
c13 = p7 + p8 + p9 + c12
c14 = p8 + p9 + p10 + c13
c15 = p9 + p10 + p11 + c14

c16 = p10 + p11 + c15
c17 = p11 + c16

simple again. Finally, south edge

147  142  133  122   59
304    5    4    2   57
330   10    1    1   54
351   11   23   25   26
362  747  806  880  931
c18

c18 = p11 + p12 + c17 + c16

bulk ones
c19 = p11 + p12 + p13 + c18
c20 = p12 + p13 + p14 + c19
c21 = p13 + p14 + p15 + c20

c22 = p14 + p15 + c0 + c21
c23 = p15 + c0 + c22
 */
