use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;
// part 1 = 3871
// part 2 = 44667

fn distance(p: &(i32, i32), q: &(i32, i32)) -> i32 {
    let (x, y) = p;
    let (a, b) = q;
    (x - a).abs() + (y - b).abs()
}

fn main() {
    const TIE: (i32, i32) = (i32::MAX, i32::MAX);
    let mut m = HashMap::new();
    let text = read_to_string("/tmp/data.txt").unwrap();
    let lines: Vec<_> = text.lines().collect();
    let mut input: Vec<(i32, i32)> = Vec::new();
    for line in lines {
        let ws: Vec<_> = line.split(", ").collect();
        input.push((ws[0].parse().unwrap(), ws[1].parse().unwrap()));
    }
    // let input = vec![(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)];
    for p in &input {
        // self always closest to self
        m.insert(p.to_owned(), p.to_owned());
    }
    let cutoff = 100;  // 100 for data
    for d in 1..cutoff {
        for &(xc, yc) in &input {
            for dx in 0..d+1 {
                let dy = d - dx;
                for (sx, sy) in [(1, 1), (1, -1), (-1, 1), (-1, -1)] {
                    if dx == 0 && sx == -1 || dy == 0 && sy == -1 {
                        // degeneracy
                        continue;
                    }
                    let (x, y) = (xc + sx * dx, yc + sy * dy);
                    if let Some(&(a, b)) = m.get(&(x, y)) {
                        if (a, b) == TIE {
                            continue;
                        }
                        let dd = distance(&(x, y), &(a, b));
                        if d < dd {
                            panic!();
                            // impossible because low d always comes before high d
                            // due to for loop
                        } else if d == dd {
                            m.insert((x, y), TIE);
                        }
                        // do nothing if d > dd
                    } else {
                        m.insert((x, y), (xc, yc));
                    }
                }
            }
        }
    }
    let part2 = m.keys().filter(
        |&k| 10_000 > input.iter().map(|p| distance(p, k) ).sum())
            .count();
    println!("part 2 = {}", part2);

    let mut disabled = HashSet::new();
    m.retain(|_, v| *v != TIE);
    for (p, q) in &m {
        if distance(p, q) == cutoff - 1 {
            disabled.insert(q.to_owned());
        }
    }
    m.retain(|_, v| !disabled.contains(v));

    let mut count = HashMap::new();
    for v in m.values() {
        count.entry(v).and_modify(|c| *c += 1).or_insert(1);
    }
    println!("part 1 = {}", count.values().max().unwrap());
}
