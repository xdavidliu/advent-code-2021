use std::cmp::{max, min};
use std::fs::read_to_string;

fn main() {
    let text = read_to_string("/home/xdavidliu/Documents/temp/data.txt").unwrap();
    let mut bots = Vec::new();
    for line in text.lines() {
        bots.push(Bot::from(line));
    }
    let mut best: Option<usize> = None;
    for (i, bot) in bots.iter().enumerate() {
        if best.is_none() || bot.r > bots[best.unwrap()].r {
            best = Some(i);
        }
    }
    let winner = &bots[best.unwrap()];
    let count = bots
        .iter()
        .filter(|b| winner.distance(*b) <= winner.r)
        .count();
    println!("part 1 = {count}"); // 889
    let ranges: Vec<_> = bots.iter().map(|b| get_range(corners(b))).collect();

    let mut best_c = 0;
    let mut best_d = 0;
    for (high, low) in &ranges {
        for val in [high, low] {
            let c = ranges
                .iter()
                .filter(|(low, high)| (low..=high).contains(&val))
                .count();
            if c > best_c {
                best_c = c;
                best_d = *val;
            } else if c == best_c {
                best_d = min(best_d, *val);
            }
        }
    }
    // https://www.reddit.com/r/adventofcode/comments/a8s17l/comment/ecdqzdg/
    // similar to above approach. Not generally correct, but the data is all situated in one line
    println!("part 2 = {}", best_d);  // 160646364
}

// https://stackoverflow.com/a/27535594/2990344
fn corners(bot: &Bot) -> impl Iterator<Item = Bot> + '_ {
    [
        (1, 0, 0),
        (-1, 0, 0),
        (0, 1, 0),
        (0, -1, 0),
        (0, 0, 1),
        (0, 0, -1),
    ]
    .iter()
    .map(|(dx, dy, dz)| Bot {
        x: bot.x + dx * bot.r,
        y: bot.y + dy * bot.r,
        z: bot.z + dz * bot.r,
        r: 0,
    })
}

fn get_range(bots: impl Iterator<Item = Bot>) -> (i64, i64) {
    let mut high = i64::MIN;
    let mut low = i64::MAX;
    for bot in bots {
        let dist = bot.distance_from_origin();
        high = max(high, dist);
        low = min(low, dist);
    }
    (low, high)
}

struct Bot {
    x: i64,
    y: i64,
    z: i64,
    r: i64,
}

impl Bot {
    fn from(line: &str) -> Self {
        let first = line.find('<').unwrap();
        let second = line.find('>').unwrap();
        let third = line.rfind('=').unwrap();
        let mut pos = line[first + 1..second].split(",");
        let x: i64 = pos.next().unwrap().parse().unwrap();
        let y: i64 = pos.next().unwrap().parse().unwrap();
        let z: i64 = pos.next().unwrap().parse().unwrap();
        let r: i64 = line[third + 1..].parse().unwrap();
        Self { x, y, z, r }
    }
    fn distance(&self, other: &Self) -> i64 {
        (self.x - other.x).abs() + (self.y - other.y).abs() + (self.z - other.z).abs()
    }
    fn distance_from_origin(&self) -> i64 {
        self.x.abs() + self.y.abs() + self.z.abs()
    }
}
