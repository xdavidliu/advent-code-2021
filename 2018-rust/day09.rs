use std::collections::HashMap;

fn main() {
    println!("part 1 = {}", score(468, 71843));  // 385820
    println!("part 2 = {}", score(468, 7184300));  // 3156297594
}

fn insert_after(a: i64, value: i64, prev: &mut HashMap<i64, i64>, next: &mut HashMap<i64, i64>) {
    let b = next[&a];
    prev.insert(b, value);
    next.insert(value, b);
    prev.insert(value, a);
    next.insert(a, value);
}

fn remove_after(a: i64, prev: &mut HashMap<i64, i64>, next: &mut HashMap<i64, i64>) -> i64 {
    let out = next[&a];
    let after = next[&out];
    next.insert(a, after);
    prev.insert(after, a);
    prev.remove(&out);
    next.remove(&out);
    out
}

fn score(players: usize, last: i64) -> i64 {
    // can't use linked list; it has cursor but that's a nightly experimental feature
    let mut next = HashMap::new();
    let mut prev = HashMap::new();
    next.insert(0, 0);
    prev.insert(0, 0);
    let mut cur = 0;
    let mut scores = vec![0; players];
    let mut p = 0usize;
    for marble in 1..1+last {
        if marble % 1_000_000 == 0 {
            println!("working on {}", marble);
        }
        if 0 != marble % 23 {
            // put marble between a and b
            let a = next[&cur];
            insert_after(a, marble, &mut prev, &mut next);
            cur = marble;
        } else {
            scores[p] += marble;
            for _ in 0..6 {
                cur = prev.get(&cur).unwrap().to_owned();
            }
            let cur_prev_twice = prev[&prev[&cur]];
            scores[p] += remove_after(cur_prev_twice, &mut prev, &mut next);
        }
        p = (p + 1) % players;
    }
    scores.iter().max().unwrap().to_owned()
}
