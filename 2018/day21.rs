use std::collections::HashSet;

fn main() {
    let mut r3: u64 = 0;
    let mut seen = HashSet::new();
    let mut seen_vec = Vec::new();
    let mut part1 = false;
    while !seen.contains(&r3) {
        seen.insert(r3);
        seen_vec.push(r3);
        let mut r2 = r3 | 65536;
        r3 = f(14070682, r2);
        while 256 <= r2 {
            r2 = 1 + (r2 - 256) / 256;
            r3 = f(r3, r2);
        }
        if !part1 {
            println!("part 1 = {r3}");  // 6132825
            part1 = true;
        }
    }
    println!("part 2 = {}", seen_vec.last().unwrap());  // 8307757
}

fn f(a: u64, b: u64) -> u64 {
    (((a + (b & 255)) & 16777215) * 65899) & 16777215
}

/*
simplified program

5  r3 = 0
6  r2 = r3 | 65536
7  r3 = 14070682
8
9  r3 = (((r3 + (r2 & 255)) & 16777215) * 65899) & 16777215
10
11
12
13 if (256 > r2) { goto 28 } else { goto 17 }
14
15
16
17 r2 = smallest x such that 256 * x > r2 - 256
18 goto 8
19
20
21
22
23
24
25
26
27
28
29 if r3 == r0 { done } else { goto 6 }
30

first r3 =


256 * x > r2 - 256

suppose r2 - 256 = 0 mod 256, then x = (r2 - 256) / 256 + 1
suppose != 0, then x = ...

so x = 1 + floordiv(r2 - 256, 256)


f(a, b) = (((a + (b & 255)) & 16777215) * 65899) & 16777215


r3 = 0
do {
    r2 = r3 | 65536
    r3 = 14070682

    r3 = f(r3, r2)

    while (256 <= r2) {
        r2 = 1 + (r2 - 256) // 256
        r3 = f(r3, r2)
    }
} while r3 != r0


so want r0 to be value of r3 after one iteration of the do

that's clearly 6132825

okay, so for part 2, look for cycle, then set r0 to be the one right before cycle detected
 */
