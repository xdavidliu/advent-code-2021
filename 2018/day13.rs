use crate::Direction::*;
use crate::Turn::*;
use std::collections::HashSet;
use std::fs::read_to_string;

#[derive(Copy, Clone)]
enum Turn {
    Left,
    Straight,
    Right,
}

#[derive(Copy, Clone)]
enum Direction {
    North,
    South,
    West,
    East,
}

impl Direction {
    fn from_char(ch: char) -> Self {
        match ch {
            '>' => East,
            '<' => West,
            '^' => North,
            'v' => South,
            _ => panic!(),
        }
    }
    fn turn(self, turn: &Turn) -> Direction {
        match turn {
            Straight => self,
            Left => self.left(),
            Right => self.right(),
        }
    }
    fn left(self) -> Direction {
        // https://doc.rust-lang.org/rust-by-example/custom_types/enum/enum_use.html
        match self {
            North => West,
            South => East,
            West => South,
            East => North,
        }
    }
    fn right(self) -> Direction {
        match self {
            North => East,
            South => West,
            West => North,
            East => South,
        }
    }
}

struct Cart {
    row: usize,
    col: usize,
    direction: Direction,
    next_turn: Turn
}

impl Cart {
    // cannot &[&[_]] -> https://stackoverflow.com/q/69036091/2990344
    fn step(&mut self, grid: &Vec<Vec<char>>) {
        match self.direction {
            East => {
                self.col += 1;
            }
            West => {
                self.col -= 1;
            }
            North => {
                self.row -= 1;
            }
            South => {
                self.row += 1;
            }
        }
        self.direction = match grid[self.row][self.col] {
            '/' => match self.direction {
                East => North,
                West => South,
                North => East,
                South => West,
            },
            '\\' => match self.direction {
                East => South,
                West => North,
                North => West,
                South => East,
            },
            '+' => {
                let prev_turn = self.next_turn;
                self.next_turn = match &self.next_turn {
                    Left => Straight,
                    Straight => Right,
                    Right => Left,
                };
                self.direction.turn(&prev_turn)
            }
            _ => self.direction,
        };
    }
}

fn main() {
    let text = read_to_string("/home/xdavidliu/Documents/temp/data.txt").unwrap();
    let grid: Vec<Vec<_>> = text.lines().map(|line| line.chars().collect()).collect();
    let mut carts = Vec::new();
    for (r, row) in grid.iter().enumerate() {
        for (c, ch) in row.iter().enumerate() {
            if "<>v^".contains(&ch.to_string()) {
                carts.push(Cart {
                    row: r,
                    col: c,
                    direction: Direction::from_char(ch.to_owned()),
                    next_turn: Left
                });
            }
        }
    }
    let mut positions: HashSet<_> = carts.iter().map(|c| (c.row, c.col)).collect();
    let mut part1_done = false;
    let mut crashed = HashSet::new();
    loop {
        carts.sort_by_key(|cart| (cart.row, cart.col));
        for cart in carts.iter_mut() {
            let before = (cart.row, cart.col);
            if crashed.contains(&before) {
                continue;
            }
            positions.remove(&before);
            cart.step(&grid);
            let after = (cart.row, cart.col);
            if positions.contains(&after) {
                if !part1_done {
                    println!("part 1 = {},{}", cart.col, cart.row);  // 109,23
                    part1_done = true;
                }
                crashed.insert((cart.row, cart.col));
                positions.remove(&after);
            } else {
                positions.insert(after);
            }
        }
        carts.retain(|c| !crashed.contains(&(c.row, c.col)));
        if carts.len() == 1 {
            let last = carts.first().unwrap();
            println!("part 2 = {},{}", last.col, last.row);  // 137,101
            return;
        }
        crashed.clear();
    }
}
