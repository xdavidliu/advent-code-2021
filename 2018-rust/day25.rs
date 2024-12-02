use std::collections::{HashSet, VecDeque};
use std::fs::read_to_string;

fn main() {
    let text = read_to_string("/home/xdavidliu/Documents/temp/data.txt").unwrap();
    let mut points = Vec::new();
    for line in text.lines() {
        let mut words = line.split(',');
        let mut xs: Vec<i32> = Vec::new();
        for _ in 0..4 {
            xs.push(words.next().unwrap().parse().unwrap());
        }
        points.push((xs[0], xs[1], xs[2], xs[3]));
    }
    let mut adj = vec![vec![]; points.len()];
    for i in 0..points.len()-1 {
        for k in i+1..points.len() {
            if near(&points[i], &points[k]) {
                adj[i].push(k);
                adj[k].push(i);
            }
        }
    }
    let mut seen = HashSet::new();
    let mut components = 0;
    for i in 0..points.len() {
        if seen.contains(&i) { continue; }
        let mut que = VecDeque::new();
        que.push_back(i);
        seen.insert(i);
        while let Some(k) = que.pop_front() {
            for &m in &adj[k] {
                if !seen.contains(&m) {
                    seen.insert(m);
                    que.push_back(m);
                }
            }
        }
        components += 1;
    }
    println!("part 1 = {components}");  // 375
}

fn near(me: &(i32, i32, i32, i32), other: &(i32, i32, i32, i32)) -> bool {
    let mut dist = 0;
    dist += (me.0 - other.0).abs();
    dist += (me.1 - other.1).abs();
    dist += (me.2 - other.2).abs();
    dist += (me.3 - other.3).abs();
    dist <= 3
}
