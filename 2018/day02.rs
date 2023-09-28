use std::collections::HashMap;
use std::fs::read_to_string;

fn main() {
    // https://stackoverflow.com/q/54056268/2990344
    // for some reason I need to separate into two statements here
    let text = read_to_string("/tmp/data.txt").unwrap();
    let lines: Vec<_> = text.lines().collect();
    part1(&lines);
    part2(&lines);
}

fn part2(lines: &Vec<&str>) {
    for i in 0..lines.len() {
        for k in i+1..lines.len() {
            let a = lines[i];
            let b = lines[k];
            match differ_by_one(a, b) {
                Some(s) => { println!("{}", s); return; }
                None => ()
            }
        }
    }
}

fn differ_by_one(a: &str, b: &str) -> Option<String> {
    if a.len() != b.len() {
        return None;
    }
    let a: Vec<_> = a.chars().collect();
    let b: Vec<_> = b.chars().collect();
    let mut mismatch = false;
    let mut chars_so_far = String::new();
    for i in 0..a.len() {
        if a[i] != b[i] {
            if mismatch {  // second mismatch found
                return None;
            } else {
                mismatch = true;
            }
        } else {
            chars_so_far.push(a[i]);
        }
    }
    return if mismatch { Some(chars_so_far) } else { None }
}

fn part1(lines: &Vec<&str>) {
    let mut count2 = 0;
    let mut count3 = 0;
    for l in lines {
        let (has2, has3) = counts(l);
        if has2 {
            count2 += 1;
        }
        if has3 {
            count3 += 1;
        }
    }
    println!("part 1 = {}", count2 * count3);
}

fn counts(s: &str) -> (bool, bool) {
    let mut m = HashMap::new();
    for c in s.chars() {
        *m.entry(c).or_insert(0) += 1;
    }
    let mut has2 = false;
    let mut has3 = false;
    for n in m.values() {
        match n {
            2 => has2 = true,
            3 => has3 = true,
            _ => ()
        }
    }
    (has2, has3)
}
