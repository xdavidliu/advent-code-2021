use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::read_to_string;

#[derive(Copy, Clone)]
struct Unit {
    kind: char,
    hp: i32,
}

fn main() {
    let path = "/home/xdavidliu/Documents/temp/data.txt";
    let mut problem = Problem::from_path(path);
    let mut i = 0;
    problem.elf_attack = 16; // trial and error lowest to get 10 elves remain
                             // note it's not monotonic! 19 gives 10 but 20 gives 9!
    while !problem.round() {
        i += 1;
    }
    let hp: i32 = problem.units.values().map(|u| u.hp).sum();
    let outcome = i * hp;
    if problem.elf_attack == ATTACK {
        println!("part 1 = {}", outcome); // 319410
    } else {
        let survive_kind = problem.units.values().map(|v| v.kind).next().unwrap();
        println!(
            "part 2: {} remain, all {}, outcome = {}",
            problem.units.len(),
            survive_kind,
            outcome  // 63168
        );
    }
}

fn adjacent_reading_order((row, col): &(usize, usize)) -> [(usize, usize); 4] {
    let row = row.to_owned();
    let col = col.to_owned();
    [
        (row - 1, col),
        (row, col - 1),
        (row, col + 1),
        (row + 1, col),
    ]
}

fn enemy_of(unit: &char) -> char {
    match unit {
        &'E' => 'G',
        &'G' => 'E',
        _ => panic!(),
    }
}

struct Problem {
    opens: HashSet<(usize, usize)>,
    units: HashMap<(usize, usize), Unit>,
    count: HashMap<char, usize>,
    elf_attack: i32,
}

const ATTACK: i32 = 3;

impl Problem {
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
            let other_unit = self.units.get_mut(&other_point).unwrap();
            let other_kind = other_unit.kind.clone();
            if &other_kind == &'G' {
                other_unit.hp -= self.elf_attack;
            } else {
                other_unit.hp -= ATTACK;
            }
            if other_unit.hp <= 0 {
                self.units.remove(&other_point);
                self.opens.insert(other_point);
                *self.count.get_mut(&other_kind).unwrap() -= 1;
            }
            return true;
        }
        false
    }
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
        let mut seen = HashSet::new();
        let mut queue = VecDeque::new();
        for start in open_adjacent {
            queue.push_back((start.to_owned(), start.to_owned(), 1));
            seen.insert(start);
        }
        let mut dist_found: Option<usize> = None;
        let mut good_targets = Vec::new();
        while let Some((next, start, dist)) = queue.pop_front() {
            if let Some(d) = dist_found {
                if dist > d {
                    break;
                }
            }
            let adj = adjacent_reading_order(&next);
            if adj
                .iter()
                .any(|x| self.units.get(x).is_some_and(|u| my_kind != u.kind))
            {
                if dist_found.is_none() {
                    dist_found = Some(dist);
                } else {
                    assert_eq!(dist, dist_found.unwrap());
                }
                good_targets.push((next, start));
            } else {
                let unseen_open: Vec<_> = self
                    .get_open_adjacent(&next)
                    .iter()
                    .filter(|&q| !seen.contains(q))
                    .copied()
                    .collect();
                for other in &unseen_open {
                    seen.insert(other.to_owned());
                    queue.push_back((other.to_owned(), start, 1 + dist));
                }
            }
        }
        good_targets.iter().min().map(|x| x.1)
    }
    fn round(&mut self) -> bool {
        let mut units_order: Vec<_> = self.units.keys().copied().collect();
        units_order.sort();
        let mut arrived = HashSet::new();
        for point in units_order {
            // FOO
            if arrived.contains(&point) {
                // forgetting this cost hours of debugging. This is edge case where
                // a unit moved right into an open spot that was just recently cleared up.
                continue;
                // it was iter 70:
                // ##............#####..GG.....####
                // ###..........#######GEEG##..####
                // ####........#########..E.#.#####
                //
                // now iter 71: Note the GEEG becoming GEG; the first E disappeared because
                // of G above.
                // ##............#####..GG.....####
                // ###..........#######.GEG##..####
                // ####........#########..E.#.#####
            }
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
                    arrived.insert(next.to_owned());
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
        let mut units = HashMap::new();
        let mut count: HashMap<char, usize> = HashMap::new();
        for (row, line) in text.lines().enumerate() {
            for (col, ch) in line.chars().enumerate() {
                let point = (row, col);
                match ch {
                    '.' => {
                        opens.insert(point);
                    }
                    '#' => {}
                    ch => {
                        units.insert(point, Unit { kind: ch, hp: 200 });
                        *count.entry(ch).or_default() += 1;
                    }
                }
            }
        }
        Self {
            opens,
            units,
            count,
            elf_attack: ATTACK,
        }
    }
}
