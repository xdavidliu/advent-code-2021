use std::collections::HashSet;
use std::fs::{OpenOptions, read_to_string};
use std::io::prelude::*;

fn insert_clay(line: &str, clay: &mut HashSet<(usize, usize)>) {
    let mut parts = line.split(", ");
    let first_part = parts.next().unwrap();
    let second_part = parts.next().unwrap();
    let first_char = first_part.chars().next().unwrap();
    let first_part_num: usize = first_part[2..].parse().unwrap();
    let mut second_part_nums = second_part[2..].split("..");
    let second_part_first_num: usize = second_part_nums.next().unwrap().parse().unwrap();
    let second_part_second_num: usize = second_part_nums.next().unwrap().parse().unwrap();
    let range = second_part_first_num..=second_part_second_num;
    if first_char == 'x' {
        for y in range {
            clay.insert((first_part_num, y));
        }
    } else {
        for x in range {
            clay.insert((x, first_part_num));
        }
    }
}

fn main() {
    let mut problem = Problem::from("/home/xdavidliu/Documents/rust_aoc/data.txt");
    problem.recurse(500, 0);
    let flow_count = problem.flow.iter().filter(|(_, y)| y >= &problem.min_y).count();
    let pool_count = problem.pool.iter().filter(|(_, y)| y >= &problem.min_y).count();
    println!("part 1 = {}", flow_count + pool_count);  // 33052
    println!("part 2 = {}", pool_count);  // 27068
    // problem.plot();
}

struct Problem {
    clay: HashSet<(usize, usize)>,
    flow: HashSet<(usize, usize)>,
    pool: HashSet<(usize, usize)>,
    min_y: usize,
    max_y: usize,
}

impl Problem {
    fn plot(&self) {
        // leave 1 to left and right
        let min_x = self.clay.iter().map(|(x, _)| x).min().unwrap().to_owned() - 2;
        let max_x = 2 + self.clay.iter().map(|(x, _)| x).max().unwrap().to_owned();
        let mut grid = vec![vec!["."; max_x - min_x + 1]; self.max_y + 1];
        // max_y + 1 because starts at 0 and includes max_y
        for &(x, y) in &self.clay {
            grid[y][x - min_x] = "#";
        }
        for &(x, y) in &self.flow {
            grid[y][x - min_x] = "|";
        }
        for &(x, y) in &self.pool {
            grid[y][x - min_x] = "~";
        }
        let mut file = OpenOptions::new()
            .write(true)
            .append(true)
            .open("/tmp/foo.txt")
            .unwrap();
        for row in &grid {
            writeln!(file, "{}", row.join(""));
        }
    }
    fn from(file_path: &str) -> Self {
        let mut clay = HashSet::new();
        for line in read_to_string(file_path).unwrap().lines() {
            insert_clay(line, &mut clay);
        }
        let min_y = clay.iter().map(|(_, y)| y).min().unwrap().to_owned();
        let max_y = clay.iter().map(|(_, y)| y).max().unwrap().to_owned();
        Self {
            clay,
            flow: HashSet::new(),
            pool: HashSet::new(),
            min_y,
            max_y,
        }
    }
    fn is_clay(&self, x: usize, y: usize) -> bool {
        self.clay.contains(&(x, y))
    }
    fn is_pool(&self, x: usize, y: usize) -> bool {
        self.pool.contains(&(x, y))
    }
    fn is_flow(&self, x: usize, y: usize) -> bool {
        self.flow.contains(&(x, y))
    }
    fn is_sand(&self, x: usize, y: usize) -> bool {
        !(self.is_clay(x, y) || self.is_flow(x, y) || self.is_pool(x, y))
    }
    fn recurse(&mut self, source_x: usize, source_y: usize) {
        let mut y = source_y;
        while y != self.max_y && self.is_sand(source_x, y + 1) {
            y += 1;
        }
        if y == self.max_y || self.is_flow(source_x, y + 1) {
            for i in source_y + 1..=y {
                self.flow.insert((source_x, i));
            }
            return;
        }
        while y != source_y {
            let mut x = source_x;
            // The is_pool is for layers of pool above the first on bottom.
            while self.is_sand(x - 1, y)
                && (self.is_clay(x - 1, y + 1) || self.is_pool(x - 1, y + 1))
            {
                x -= 1;
            }
            let left_x = x;
            x = source_x;
            while self.is_sand(x + 1, y)
                && (self.is_clay(x + 1, y + 1) || self.is_pool(x + 1, y + 1))
            {
                x += 1;
            }
            let right_x = x;
            if self.is_clay(left_x - 1, y) && self.is_clay(right_x + 1, y) {
                for x in left_x..=right_x {
                    self.pool.insert((x, y));
                }
                y -= 1;
                continue;
            } else {
                if self.is_sand(left_x - 1, y) {
                    self.recurse(left_x - 1, y);
                }
                if self.is_sand(right_x + 1, y) {
                    self.recurse(right_x + 1, y);
                }
                if self.is_pool(left_x - 1, y + 1) || self.is_pool(right_x + 1, y + 1) {
                    continue;  // keeping same y
                } else {  // done
                    if self.is_sand(left_x - 1, y) {
                        self.flow.insert((left_x - 1, y));
                    }
                    if self.is_sand(right_x + 1, y) {
                        self.flow.insert((right_x + 1, y));
                    }
                    for x in left_x..=right_x {
                        self.flow.insert((x, y));
                    }
                    for i in source_y + 1..=y - 1 {
                        self.flow.insert((source_x, i));
                    }
                    return;
                }
            }
        }
    }
}
