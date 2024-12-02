use std::collections::{HashSet, VecDeque};
use std::fs::read_to_string;
use std::mem::swap;

fn main() {
    let data = read_to_string("/home/xdavidliu/Documents/temp/data.txt").unwrap();
    let data: Vec<char> = data.chars().collect();
    let (segs, _) = parse_multiple_segments(&data, 1);
    let mut points = HashSet::new();
    points.insert((0, 0));
    let mut doors = HashSet::new();
    process(&segs, &mut doors, &mut points);
    bfs(&doors);
}

fn bfs(doors: &HashSet<(i32, i32)>) {
    let mut que = VecDeque::new();
    que.push_back(((0, 0), 0));
    let mut seen = HashSet::new();
    let mut last_d = 0;
    let mut count = 0;
    while let Some(((x, y), d)) = que.pop_front() {
        last_d = d;
        if d >= 1000 {
            count += 1;
        }
        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)] {
            if doors.contains(&(x + dx, y + dy)) {
                let (xp, yp) = (x + 2 * dx, y + 2 * dy);
                if seen.contains(&(xp, yp)) {
                    continue;
                }
                seen.insert((xp, yp));
                que.push_back(((xp, yp), d + 1));
            }
        }
    }
    println!("part 1 = {}", last_d);  // 3788
    println!("part 2 = {}", count);  // 8568
}

fn evolve(point: &(i32, i32), s: &str, doors: &mut HashSet<(i32, i32)>) -> (i32, i32) {
    let (mut x, mut y) = point.clone();
    for ch in s.chars() {
        match ch {
            'N' => {
                doors.insert((x, y + 1));
                y += 2;
            }
            'W' => {
                doors.insert((x - 1, y));
                x -= 2;
            }
            'E' => {
                doors.insert((x + 1, y));
                x += 2;
            }
            'S' => {
                doors.insert((x, y - 1));
                y -= 2;
            }
            _ => panic!()
        }
    }
    return (x, y);
}

fn process(segs: &[Segment], doors: &mut HashSet<(i32, i32)>, points: &mut HashSet<(i32, i32)>) {
    // if segs.is_empty() { return; }  // does nothing
    let mut next = HashSet::new();
    for seg in segs {
        next.clear();
        match seg {
            Segment::Raw(s) => {
                // interesting that this iter() is needed for &mut
                for point in points.iter() {
                    next.insert(evolve(point, s, doors));
                }
            }
            Segment::Paren(z) => {
                for branch in z {
                    let mut branch_points = points.clone();
                    process(branch, doors, &mut branch_points);
                    next.extend(branch_points.iter());
                }
            }
        }
        swap(points, &mut next);
    }

}

fn parse_multiple_segments(cs: &[char], i: usize) -> (Vec<Segment>, usize) {
    let mut i = i;
    let mut v = Vec::new();
    while !"$|)".contains(cs[i]) {
        let (seg, k) = parse_single_segment(cs, i);
        v.push(seg);
        i = k; // ??? todo
    }
    (v, i)
}

// returns index after last char of the segment
fn parse_single_segment(cs: &[char], i: usize) -> (Segment, usize) {
    if cs[i].is_alphabetic() {
        let mut s = String::new();
        let mut i = i;
        while cs[i].is_alphabetic() {
            s.push(cs[i]);
            i += 1;
        }
        (Segment::Raw(s), i)
    } else {
        assert_eq!(cs[i], '(');
        let mut i = i;
        let mut v = Vec::new();
        while cs[i] != ')' {
            let (seg_vec, k) = parse_multiple_segments(cs, i + 1);
            v.push(seg_vec);
            i = k;
        }
        (Segment::Paren(v), i + 1)
    }
}

enum Segment {
    Raw(String),
    Paren(Vec<Vec<Segment>>),
}
