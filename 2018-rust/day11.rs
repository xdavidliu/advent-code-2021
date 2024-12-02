use std::cmp;

fn main() {
    let grid = create_grid();
    part1(&grid);  // 20,37
    part2(&grid);  // 90,169,15
}

fn create_grid() -> Vec<Vec<i32>> {
    let serial = 7689;  // example 8, data 7689
    let mut grid = vec![vec![0; 300]; 300];
    for y in 1..1+300 {
        for x in 1..1+300 {
            let rack_id = x + 10;
            let mut power = rack_id * y + serial;
            power *= rack_id;
            power = (power % 1000) / 100 - 5;
            let r = y as usize - 1;
            let c = x as usize - 1;
            grid[r][c] = power;
        }
    }
    grid
}

fn part2(grid: &Vec<Vec<i32>>) {
    let mut best = i32::MIN;
    let mut best_x = 0;
    let mut best_y = 0;
    let mut best_size = 0;
    for y in 1..1+300 {
        for x in 1..1+300 {
            let mut sum = 0;
            for size in 1..cmp::min(300+1-y, 300+1-x) {
                // the last row/col of a square with size at (x,y)
                let r_below = y - 1 + size - 1;
                let c_right = x - 1 + size - 1;
                sum += grid[r_below][c_right];
                for d in 0..size-1 {
                    let r = y - 1 + d;
                    let c = x - 1 + d;
                    sum += grid[r_below][c];
                    sum += grid[r][c_right];
                }
                if sum > best {
                    best = sum;
                    best_x = x;
                    best_y = y;
                    best_size = size;
                }
            }
        }
    }
    println!("part 2 = {},{},{}", best_x, best_y, best_size);
}

fn part1(grid: &Vec<Vec<i32>>) {
    let mut best = i32::MIN;
    let mut best_x = 0;
    let mut best_y = 0;
    for y in 1..1+300-2 {
        for x in 1..1+300-2 {
            let mut sum = 0;
            for dx in 0..3 {
                for dy in 0..3 {
                    let r = y as usize - 1 + dy;
                    let c = x as usize - 1 + dx;
                    sum += grid[r][c];
                }
            }
            if sum > best {
                best = sum;
                best_x = x;
                best_y = y;
            }
        }
    }
    println!("part 1 = {},{}", best_x, best_y);
}
