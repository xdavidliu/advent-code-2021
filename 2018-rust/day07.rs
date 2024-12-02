use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap};
use std::fs::read_to_string;

fn main() {
    // don't for the 60 thing for data and 5 workers below
    let mut adj = get_adjacency("/tmp/data.txt");
    let ready: BinaryHeap<_> = adj
        .iter()
        .filter(|(_, v)| v.is_empty())
        .map(|(k, _)| Reverse(k.to_owned()))
        .collect();
    adj.retain(|_, v| !v.is_empty());
    part1(&adj, &ready);
    part2(&adj, &ready, 5, 60);
}

fn part2(
    adj: &HashMap<char, Vec<char>>,
    ready: &BinaryHeap<Reverse<char>>,
    workers: usize,
    fixed_cost: i32,
) {
    let mut map = adj.clone();
    let mut ready = ready.clone();
    let mut working = BinaryHeap::new();
    let mut time = 0;
    // fill working and remaining with ready
    while !map.is_empty() || !working.is_empty() || !ready.is_empty() {
        while !ready.is_empty() && workers > working.len() {
            let next = ready.pop().unwrap().0;
            let time_done = time + fixed_cost + 1 + next as i32 - 'A' as i32;
            working.push(Reverse((time_done, next)));
        }
        let (time_done, next) = working.pop().unwrap().0;
        time = time_done;
        for (k, v) in &mut map {
            v.retain(|c| c != &next);
            if v.is_empty() {
                ready.push(Reverse(k.to_owned()));
            }
        }
        map.retain(|_, v| !v.is_empty());
    }
    println!("part 2 = {}", time);  // 1050
}

fn get_adjacency(path: &str) -> HashMap<char, Vec<char>> {
    let mut map = HashMap::new();
    read_to_string(path).unwrap().lines().for_each(|ln| {
        let v: Vec<_> = ln.chars().collect();
        map.entry(v[5]).or_insert(Vec::new()); // to easily see ready ones later
        map.entry(v[36]).or_insert(Vec::new()).push(v[5]);
    });
    map
}

fn part1(adj: &HashMap<char, Vec<char>>, ready: &BinaryHeap<Reverse<char>>) {
    let mut map = adj.clone();
    let mut ready = ready.clone();
    let mut ans = String::new();
    while !ready.is_empty() {
        map.retain(|_, v| !v.is_empty());
        let next = ready.pop().unwrap().0;
        ans.push(next);
        for (k, v) in &mut map {
            v.retain(|c| c != &next);
            if v.is_empty() {
                ready.push(Reverse(k.to_owned()));
            }
        }
    }
    println!("part 1 = {}", ans); // GJFMDHNBCIVTUWEQYALSPXZORK
}
