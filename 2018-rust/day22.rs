use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet};

const DEPTH: i32 = 3066;
const TARGET: (i32, i32) = (13, 726);

// convention: erosion value cannot this
const NEITHER: i32 = 0;
const TORCH: i32 = 1;
const CLIMB: i32 = 2;

fn main() {
    let mut erosion = HashMap::new();
    for sum in 0..=1200 {
        // reasonable upper limit
        for x in 0..=sum {
            let y = sum - x;
            let geo = geologic_index(x, y, &erosion);
            erosion.insert((x, y), (geo + DEPTH) % 20183);
        }
    }
    let mut sum = 0;
    for x in 0..=TARGET.0 {
        for y in 0..=TARGET.1 {
            sum += erosion.get(&(x, y)).unwrap() % 3;
        }
    }
    println!("part 1 = {sum}"); // 10115
    let mut heap = BinaryHeap::new();
    heap.push(Reverse((0, TORCH, (0, 0))));
    let mut done = HashSet::new();
    while let Some(Reverse((time, tool, (x, y)))) = heap.pop() {
        if tool == TORCH && (x, y) == TARGET {
            println!("part 2 = {time}");  // 990
            return;
        } else if done.contains(&(tool, (x, y))) {
            continue;
        } else {
            done.insert((tool, (x, y)));
        }
        let this_erosion = erosion.get(&(x, y)).unwrap().to_owned();
        for (dx, dy) in [(1i32, 0i32), (-1, 0), (0, 1), (0, -1)] {
            let (u, v) = (x + dx, y + dy);
            if u < 0 || v < 0 {
                continue;
            }
            let next_erosion = erosion.get(&(u, v)).unwrap().to_owned();
            for next_tool in [NEITHER, TORCH, CLIMB] {
                // see convention for why ==
                if next_tool == this_erosion % 3
                    || next_tool == next_erosion % 3
                    || done.contains(&(next_tool, (u, v)))
                {
                    continue;
                }
                let next_time = if tool == next_tool {
                    time + 1
                } else {
                    time + 8
                };
                heap.push(Reverse((next_time, next_tool, (u, v))));
            }
        }
    }
}

fn geologic_index(x: i32, y: i32, erosion: &HashMap<(i32, i32), i32>) -> i32 {
    if [(0, 0), TARGET].contains(&(x, y)) {
        0
    } else if y == 0 {
        x * 16807
    } else if x == 0 {
        y * 48271
    } else {
        erosion.get(&(x - 1, y)).unwrap() * erosion.get(&(x, y - 1)).unwrap()
    }
}
