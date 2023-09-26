use std::collections::HashSet;
use std::fs::read_to_string;

fn main() {
    let xs: Vec<_> = read_to_string("/tmp/data.txt").unwrap().lines()
        .map(|s| { s.parse::<i32>().unwrap() }).collect();
    let mut s = 0;
    let mut part1_done = false;
    let mut part2_done = false;
    let mut seen = HashSet::new();
    while !(part1_done && part2_done) {
        for x in &xs {
            s += x;
            if !part2_done && seen.contains(&s) {
                part2_done = true;
                println!("part 2 = {}", s);  // 481
            }
            if !part2_done {
                seen.insert(s);
            }
        }
        if !part1_done {
            println!("part 1 = {}", s);  // 402
            part1_done = true
        }
    }
}
