use std::collections::HashSet;
use std::fs::read_to_string;

fn main() {
    let s = read_to_string("/tmp/data.txt").unwrap();
    part1(&s);  // 9808
    part2(&s);  // 6484
}

fn part1(s: &str) {
    println!("part 1 = {}", react_length(&s));
}

fn part2(s: &str) {
    let seen: HashSet<_> = s
        .chars()
        .map(|x| x.to_lowercase().next().unwrap())
        .collect();
    let ans = seen
        .iter()
        .map(|c| react_length(&without_one(s, c)))
        .min()
        .unwrap();
    println!("part 2 = {}", ans);
}

fn without_one(s: &str, c: &char) -> String {
    s.chars().filter(|x| !x.eq_ignore_ascii_case(c)).collect()
}

fn react_length(s: &str) -> usize {
    let mut v = String::new();
    // filter because input may have newline at end
    for c in s.chars().filter(|x| x.is_alphabetic()) {
        if !v.is_empty() && will_destroy(c, v.chars().last().unwrap()) {
            v.pop();
        } else {
            v.push(c);
        }
    }
    v.len()
}

fn will_destroy(a: char, b: char) -> bool {
    a.is_lowercase() != b.is_lowercase() && a.eq_ignore_ascii_case(&b)
}
