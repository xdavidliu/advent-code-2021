use std::collections::HashSet;
use lazy_static::lazy_static;
use regex::Regex;
use std::fs::read_to_string;
extern crate lazy_static;
// in cargo.toml dependencies put
// regex = "1.9.6"

fn main() {
    let mut v = read_points("/tmp/data.txt");
    let part2 = 10_813;  // obtained by trial and error
    println!("part 2 = {}", part2);
    for i in 0..10_813 {
        for p in &mut v {
            p.update();
        }
        // find the range by trial and error
        // 10804 to 10818
        /*
        if (10_790..10_830).contains(&i) && i % 2 == 0 {
            let x_min = v.iter().map(|p| p.x).min().unwrap();
            let x_max = v.iter().map(|p| p.x).max().unwrap();
            let y_min = v.iter().map(|p| p.y).min().unwrap();
            let y_max = v.iter().map(|p| p.y).max().unwrap();
            println!("iter {} -> {} -- {}", i, x_max - x_min, y_max - y_min);
        }
         */
    }
    let x_min = v.iter().map(|p| p.x).min().unwrap();
    let x_max = v.iter().map(|p| p.x).max().unwrap();
    let y_min = v.iter().map(|p| p.y).min().unwrap();
    let y_max = v.iter().map(|p| p.y).max().unwrap();
    let set: HashSet<(i32, i32)> = v.iter().map(|p| (p.x, p.y)).collect();
    println!("part 1 below:");  // ERCXLAJL
    for y in (y_min..1+y_max) {
        let mut line = String::new();
        for x in (x_min..1+x_max) {
            line.push(if set.contains(&(x, y)) {'#'} else {'.'});
        }
        println!("{}", line);
    }
}

fn read_points(path: &str) -> Vec<Point> {
    read_to_string(path)
        .unwrap()
        .lines()
        .map(|ln| to_point(ln))
        .collect()
}

lazy_static! {
    static ref POINT_REGEX: Regex =
        Regex::new(r"position=<(.+), (.+)> velocity=<(.+), (.+)>").unwrap();
}

fn to_point(ln: &str) -> Point {
    let cap = POINT_REGEX.captures(ln).unwrap();
    let v: Vec<i32> = (1..5)
        .map(|i| cap.get(i).unwrap().as_str().trim().parse().unwrap())
        .collect();
    Point {
        x: v[0],
        y: v[1],
        vx: v[2],
        vy: v[3],
    }
}

struct Point {
    x: i32,
    y: i32,
    vx: i32,
    vy: i32,
}

// https://doc.rust-lang.org/rust-by-example/fn/methods.html
impl Point {
    fn update(&mut self) {
        self.x += self.vx;
        self.y += self.vy;
    }
}
