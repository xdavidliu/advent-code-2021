use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::read_to_string;

// part 1 = 303750 too low
// 320166 not correct
// 320166
// claims 319410

fn main() {
    foo2();
}

#[derive(Copy, Clone)]
struct Unit {
    kind: char,
    hp: i32,
}

fn show(problem: &Problem) {
    let mut grid = vec![vec!['a'; problem.cols]; problem.rows];
    for (r, c) in &problem.walls {
        grid[*r][*c] = '#';
    }
    for (r, c) in &problem.opens {
        grid[*r][*c] = '.';
    }
    for ((r, c), unit) in &problem.units {
        grid[*r][*c] = unit.kind;
    }
    for row in grid {
        let s: String = row.iter().collect();
        println!("{}", s);
    }
}

fn foo3() {
    let path = "/home/xdavidliu/Documents/temp/test.txt";
    let mut problem = Problem::from_path(path);
    let (x, y) = problem.get_step(&(5, 7)).unwrap();
    println!("{} {}", x, y);
    println!("want 6, 7");
}

fn foo2() {
    let path = "/home/xdavidliu/Documents/temp/data.txt";
    let mut problem = Problem::from_path(path);
    let mut i = 0;
    while i < i32::MAX && !problem.round() {
        i += 1;
    }
    let hp: i32 = problem.units.values().map(|u| u.hp).sum();
    println!("part 1 = {}", i * hp);
    // println!("i = {}, hp = {}", i, hp);
    // show(&problem);
}

struct Problem {
    opens: HashSet<(usize, usize)>,
    walls: HashSet<(usize, usize)>,
    units: HashMap<(usize, usize), Unit>,
    count: HashMap<char, usize>,
    rows: usize, // for debugging
    cols: usize,
}

fn adjacent_reading_order((row, col): &(usize, usize)) -> [(usize, usize); 4] {
    // north, west, east, south
    [
        (row - 1, col.to_owned()),
        (row.to_owned(), col - 1),
        (row.to_owned(), col + 1),
        (row + 1, col.to_owned()),
    ]
}

fn enemy_of(unit: &char) -> char {
    match unit {
        &'E' => 'G',
        &'G' => 'E',
        _ => panic!(),
    }
}

impl Problem {
    const ATTACK: i32 = 3;
    // returns whether all done (true) or still more rounds to go (false)
    fn first_enemy(&self, from: &(usize, usize)) -> Option<(usize, usize)> {
        let mut candidates = Vec::new();
        let my_kind = self.units.get(from).unwrap().kind;
        for point in adjacent_reading_order(from) {
            if let Some(unit) = self.units.get(&point) {
                if my_kind != unit.kind {
                    candidates.push((unit.hp, point.to_owned()));
                }
            }
        }
        candidates.iter().min().map(|p| p.1)
    }
    fn try_attack(&mut self, from: &(usize, usize)) -> bool {
        if let Some(other_point) = self.first_enemy(from) {
            let my_kind = self.units.get(from).unwrap().kind;
            let other_unit = self.units.get_mut(&other_point).unwrap();
            if my_kind != other_unit.kind {
                other_unit.hp -= Problem::ATTACK;
                if other_unit.hp <= 0 {
                    self.units.remove(&other_point);
                    self.opens.insert(other_point.to_owned());
                    *self.count.get_mut(&enemy_of(&my_kind)).unwrap() -= 1;
                }
            }
            return true;
        }
        false
    }
    // note cannot just BFS with reading order because a target could be earlier
    // in reading order but reachable with a first step later in reading order.
    // idea: collect the empty adjacent positions and pass to get_step
    fn get_open_adjacent(&self, from: &(usize, usize)) -> Vec<(usize, usize)> {
        adjacent_reading_order(from)
            .iter()
            .filter(|&p| self.opens.contains(p))
            .copied()
            .collect()
    }
    fn get_step(&self, from_unit_point: &(usize, usize)) -> Option<(usize, usize)> {
        let open_adjacent = self.get_open_adjacent(from_unit_point);
        let my_kind = self.units.get(from_unit_point).unwrap().kind;
        let mut all_targets = HashSet::new();
        for (k, v) in &self.units {
            if v.kind != my_kind {
                for x in self.get_open_adjacent(k) {
                    all_targets.insert(x.to_owned());
                }
            }
        }
        let mut seen = HashSet::new();
        let mut queue = VecDeque::new();
        for start in &open_adjacent {
            if all_targets.contains(start) {
                return Some(start.to_owned());
            }
            queue.push_back((start.to_owned(), start.to_owned(), 1));
            seen.insert(start.to_owned());
        }
        let mut dist_found: Option<usize> = None;
        let mut good_targets = Vec::new();
        while let Some((next, start, dist)) = queue.pop_front() {
            if let Some(dnt) = dist_found {
                if dist == dnt {
                    break;
                }
            }
            let unseen_open: Vec<_> = self
                .get_open_adjacent(&next)
                .iter()
                .filter(|&q| !seen.contains(q)).copied().collect();
            for other in &unseen_open
            {
                seen.insert(other.to_owned());
                if all_targets.contains(other) {
                    if dist_found.is_none() {
                        dist_found = Some(dist + 1);
                    } else {
                        assert_eq!(dist + 1, dist_found.unwrap());
                    }
                    good_targets.push((other.to_owned(), start.to_owned()));
                } else {
                    queue.push_back((other.to_owned(), start, 1 + dist));
                }
            }
        }
        good_targets.iter().min().map(|x| x.1)
    }
    fn round(&mut self) -> bool {
        let mut units_order: Vec<_> = self.units.keys().copied().collect();
        units_order.sort();
        for point in units_order {
            // still exists, i.e. not yet destroyed by a previous iteration of for
            if let Some(unit) = self.units.get(&point).copied() {
                if &0 == self.count.get(&enemy_of(&unit.kind)).unwrap() {
                    // no enemies; all done
                    return true;
                }
                if self.try_attack(&point) {
                    continue;
                }
                if let Some(next) = self.get_step(&point) {
                    let to_move = self.units.remove(&point).unwrap();
                    self.opens.insert(point.to_owned());
                    self.opens.remove(&next);
                    self.units.insert(next.to_owned(), to_move);
                    self.try_attack(&next); // can attack after moving!
                }
            }
        }
        false
    }
    fn from_path(path: &str) -> Self {
        let text = read_to_string(path).unwrap();
        let mut opens = HashSet::new();
        let mut walls = HashSet::new();
        let mut units = HashMap::new();
        let mut count: HashMap<char, usize> = HashMap::new();
        let mut rows = 0;
        let mut cols = 0;
        for (row, line) in text.lines().enumerate() {
            rows += 1;
            cols = 0;
            for (col, ch) in line.chars().enumerate() {
                cols += 1;
                let point = (row, col);
                match ch {
                    '.' => {
                        opens.insert(point);
                    }
                    '#' => {
                        walls.insert(point);
                    }
                    ch => {
                        units.insert(point, Unit { kind: ch, hp: 200 });
                        *count.entry(ch).or_default() += 1;
                    }
                }
            }
        }
        Self {
            opens,
            walls,
            units,
            count,
            rows,
            cols,
        }
    }
}
