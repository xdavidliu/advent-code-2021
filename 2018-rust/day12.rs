use std::collections::HashMap;
use std::fs::read_to_string;
use std::mem::swap;

fn score(s: &str, start: i64) -> i64 {
    s.chars()
        .enumerate()
        .map(|(k, v)| if v == '#' { k as i64 + start } else { 0 })
        .sum()
}

fn main() {
    let text = read_to_string("/tmp/data.txt").unwrap();
    let lines: Vec<_> = text.lines().collect();
    // replace to easily trim
    let mut next = String::from(&lines[0][15..].replace(".", " "));
    let mut start: i64 = 0;
    // assume next[0] == '#', otherwise gotta trim and set start to position of first #
    let mut now = String::new();
    let mut rule = HashMap::new();
    for &line in &lines[2..] {
        let words: Vec<_> = line.split(" => ").collect();
        let ch = if words[1].chars().next().unwrap() == '#' {
            '#'
        } else {
            ' '
        };
        rule.insert(words[0].replace(".", " "), ch);
    }
    let mut seen = HashMap::new();
    seen.insert(next.to_owned(), 0);
    for i in 1..160 {
        swap(&mut next, &mut now);
        next.clear();
        for m in (1..5).rev() {
            let k = " ".repeat(m).to_owned() + &now[..5 - m];
            next.push(rule.get(&k).unwrap_or(&' ').to_owned());
        }
        for i in 0..now.len() - 5 + 1 {
            next.push(rule.get(&now[i..i + 5]).unwrap_or(&' ').to_owned());
        }
        for m in (1..5).rev() {
            let k = now[now.len() - m..now.len()].to_owned() + &" ".repeat(5 - m);
            next.push(rule.get(&k).unwrap_or(&' ').to_owned());
        }
        start += next.find("#").unwrap() as i64 - 3 + 1;
        next = next.trim().to_owned();
        if let Some(&p) = seen.get(&next) {
            // println!("debug: cycle detected from {} to {}", p, i);
            // return;
        }
        if i == 20 {
            println!("part 1 = {}", score(&next, start)); // 4818
        }
        if i > 148 {
            // println!("debug {} {} {}", i, start, next);
        }
        // don't forget, start can shift even if seen cyclic
        seen.insert(next.to_owned(), i);
    }
    // use the debug print statements above and see that...
    // hits a fixed point after i = 150, with start = 80. Each subsequent iter
    // has start increase by 1, hence at i = 50_000_000_000, we have start = that minus 70
    println!("part 2 = {}", score(&next, 50_000_000_000 - 70));
    // 5100000001377
}
