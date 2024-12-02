use std::fs::read_to_string;

fn main() {
    let v = get_input("/tmp/data.txt");
    let (_, sum) = part1(&v, 0);
    println!("part 1 = {}", sum); // 40977
    let (_, sum) = part2(&v, 0);
    println!("part 2 = {}", sum); // 27490
}

fn get_input(path: &str) -> Vec<usize> {
    read_to_string(path)
        .unwrap()
        .trim()
        .split(" ")
        .map(|w| w.parse().unwrap())
        .collect()
}

fn part1(xs: &[usize], i: usize) -> (usize, usize) {
    let n = xs[i];
    let m = xs[i + 1];
    let mut i = i + 2;
    let mut sum: usize = 0;
    for _ in 0..n {
        let mut d: usize = 0;
        (i, d) = part1(xs, i);
        sum += d;
    }
    for k in i..i + m {
        sum += xs[k];
    }
    (i + m, sum)
}

fn part2(xs: &[usize], i: usize) -> (usize, usize) {
    let n = xs[i];
    let m = xs[i + 1];
    let mut i = i + 2;
    let mut vals = Vec::new();
    for _ in 0..n {
        let mut d: usize = 0;
        (i, d) = part2(xs, i);
        vals.push(d);
    }
    let mut sum: usize = 0;
    if n == 0 {
        for k in i..i + m {
            sum += xs[k];
        }
    } else {
        for k in i..i + m {
            let ind = xs[k]-1;
            if (0..vals.len()).contains(&ind) {
                sum += vals[ind];
            }
        }
    }
    (i + m, sum)
}
