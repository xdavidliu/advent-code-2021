use std::collections::HashSet;
use std::fs::read_to_string;

use regex::Regex;
// in cargo.toml dependencies put
// regex = "1.9.5"

/// rust std no regex, need crate
// https://users.rust-lang.org/t/why-rust-doesnt-include-the-regular-expression-library-as-the-standard-library/24134

fn main() {
    // ^ and $ is for it to match entire
    // https://github.com/rust-lang/regex/discussions/737
    let pat = r"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)";   // #11 @ 755,237: 24x22
    let reg = Regex::new(pat).unwrap();
    let text = read_to_string("/tmp/data.txt").unwrap();
    let lines: Vec<_> = text.lines().collect();
    let mut once = HashSet::new();
    let mut multiple = HashSet::new();
    let mut tups = Vec::new();
    for line in lines {
        let caps = reg.captures(line).unwrap();
        let id: i32 = caps.get(1).unwrap().as_str().parse().unwrap();
        let row: i32 = caps.get(2).unwrap().as_str().parse().unwrap();
        let col: i32 = caps.get(3).unwrap().as_str().parse().unwrap();
        let nr: i32 = caps.get(4).unwrap().as_str().parse().unwrap();
        let nc: i32 = caps.get(5).unwrap().as_str().parse().unwrap();
        tups.push((id, row, col, nr, nc));
    }
    for (_, row, col, nr, nc) in &tups {
        for r in *row..*row+nr {
            for c in *col..*col+nc {
                let p = &(r, c);
                if once.remove(p) {
                    multiple.insert(p.to_owned());
                } else if !multiple.contains(p) {  // neither in once nor multiple
                    once.insert(p.to_owned());
                }
            }
        }
    }
    println!("part 1 = {}", multiple.len());  // 112378
    for (id, row, col, nr, nc) in tups {
        let mut overlap = false;
        'outer: for r in row..row+nr {
            for c in col..col+nc {
                let p = (r, c);
                if multiple.contains(&p) {
                    overlap = true;
                    break 'outer;
                }
            }
        }
        if !overlap {
            println!("part 2 = {}", id);  // 603
            return;
        }
    }
}
