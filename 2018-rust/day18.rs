use std::collections::HashMap;
use std::fs::read_to_string;

fn main() {
    let mut grid = Vec::new();
    for line in read_to_string("/home/xdavidliu/Documents/rust_aoc/data.txt").unwrap().lines() {
        let row: Vec<char> = line.chars().collect();
        grid.push(row);
    }
    let mut next = vec![vec!['a'; grid[0].len()]; grid.len()];
    let mut seen = HashMap::new();
    let mut seen_vec = Vec::new();
    let grid_string = to_string(&grid);
    seen.insert(grid_string.clone(), 0);
    seen_vec.push(grid_string);
    for k in 0..1000000 {  // any large number works here
        for i in 0..grid.len() {
            for j in 0..grid[0].len() {
                next[i][j] = change(&grid, i, j);
            }
        }
        std::mem::swap(&mut grid, &mut next);
        let grid_string = to_string(&grid);
        if let Some(prev) = seen.get(&grid_string) {
            let n = 1_000_000_000;  // from part 2
            let r = (n - prev) % (k + 1 - prev);
            println!("part 2 = {}", answer(&seen_vec[prev + r]));
            return;
            /*
            after prev iters you had s
            after k+1 iters you had s
            want value after N = 1000000 iters
            so value after prev iters, then another N - prev iters
            k+1 - prev = P is the period
            (N - prev) % P = r
            want r iters after prev, i.e. prev + r
             */
        } else {
            seen.insert(grid_string.clone(), k + 1);
            seen_vec.push(grid_string);
        }
        if k + 1 == 10 {
            println!("part 1 = {}", answer(&to_string(&grid)));
        }
    }
}

fn answer(grid_string: &str) -> i32 {
    let mut c_tree = 0;
    let mut c_lumber = 0;
    for ch in grid_string.chars() {
        match ch {
            '|' => { c_tree += 1; }
            '#' => { c_lumber += 1; }
            _ => ()
        }
    }
    c_tree * c_lumber
}

fn to_string(grid: &Vec<Vec<char>>) -> String {
    let mut out = String::new();
    for row in grid {
        let s: String = row.iter().collect();
        out.push_str(&s);
    }
    out
}

fn change(grid: &Vec<Vec<char>>, row: usize, col: usize) -> char {
    let (_, c_tree, c_lumber) = adj_counts(&grid, row as i32, col as i32);
    match grid[row][col] {
        '.' => if c_tree >= 3 { '|' } else { '.' }
        '|' => if c_lumber >= 3 { '#' } else { '|' }
        '#' => if c_lumber >= 1 && c_tree >= 1 { '#' } else { '.' }
        _ => panic!()
    }
}

fn adj_counts(grid: &Vec<Vec<char>>, row: i32, col: i32) -> (usize, usize, usize) {
    let ds = [(1, 1), (1, 0), (1, -1), (0, 1), (0, -1), (-1, 1), (-1, 0), (-1, -1)];
    let mut c_open = 0;
    let mut c_tree = 0;
    let mut c_lumber = 0;
    for (dr, dc) in ds {
        let i = row + dr;
        let j = col + dc;
        if !(0..grid.len() as i32).contains(&i) {
            continue;
        }
        if !(0..grid[0].len() as i32).contains(&j) {
            continue;
        }
        match grid[i as usize][j as usize] {
            '.' => { c_open += 1; }
            '|' => { c_tree += 1; }
            '#' => { c_lumber += 1; }
            _ => panic!()
        }
    }
    (c_open, c_tree, c_lumber)
}
