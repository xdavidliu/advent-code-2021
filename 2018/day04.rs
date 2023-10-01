use std::collections::HashMap;
use std::fs::read_to_string;
use lazy_static::lazy_static;
extern crate lazy_static;
use regex::Regex;
// in cargo.toml dependencies put
// regex = "1.9.5"
// lazy_static = "1.4.0"

enum Record {
    Sleep(Time),
    Wake(Time),
    Begin(Time, u32)
}

struct Time {
    minutes: u32
}

// https://docs.rs/regex/latest/regex/#avoid-re-compiling-regexes-especially-in-a-loop
lazy_static! {
    static ref TIME_REGEX: Regex =
        Regex::new(r"\[(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d)\]").unwrap();

    static ref GUARD_REGEX: Regex =
        Regex::new(r"Guard #(\d+)").unwrap();
}

fn extract_time(s: &str) -> Time {
    let caps = TIME_REGEX.captures(s).unwrap();
    Time {
        minutes: caps.get(5).unwrap().as_str().parse().unwrap()
    }
}

fn extract_guard(s: &str) -> u32 {
    GUARD_REGEX.captures(s).unwrap().get(1).unwrap().as_str().parse().unwrap()
}

// note rust crate regex doesn't match entire string, so capture works
// as long as any match is found
// [1518-07-18 00:46] wakes up
fn record_from_string(s: &str) -> Record {
    let t = extract_time(s);
    if s.ends_with("wakes up") {
        Record::Wake(t)
    } else if s.ends_with("falls asleep") {
        Record::Sleep(t)
    } else if s.ends_with("begins shift") {
        Record::Begin(t, extract_guard(s))
    } else {
        panic!();
    }
}

fn read_records() -> Vec<Record> {
    let text = read_to_string("/tmp/data.txt").unwrap();
    let mut lines: Vec<_> = text.lines().collect();
    lines.sort();  // because lex-sorting the string already correctly orders by datetime
    lines.iter().map(|x| record_from_string(x)).collect()
}

fn most_sleep(records: &[Record]) -> &u32 {
    let mut current_guard = None;
    let mut start_sleep = None;
    let mut minutes_asleep: HashMap<&u32, u32> = HashMap::new();
    for r in records {
        match r {
            Record::Begin(_, id) => {
                current_guard = Some(id);
            }
            Record::Sleep(t) => {
                start_sleep = Some(t);
            }
            Record::Wake(t) => {
                let start = start_sleep.unwrap();
                let dt = t.minutes - start.minutes;
                // https://stackoverflow.com/a/73837573/2990344
                *minutes_asleep.entry(current_guard.unwrap()).or_default() += dt;
            }
        }
    }
    let mut best = None;
    for (k, v) in minutes_asleep {
        if let Some((_, b)) = best {
            if v <= b { continue; }
        }
        best = Some((k, v));
    }
    let (best_id, _) = best.unwrap();
    best_id
}

fn main() {
    let records = read_records();
    let best = most_sleep(&records);
    let mut current_guard = None;
    let mut start_sleep = None;
    let mut counts_map = HashMap::new();
    for r in &records {
        match r {
            Record::Begin(_, id) => {
                current_guard = Some(id);
            }
            Record::Sleep(t) => {
                // if current_guard.unwrap() == best {
                //     start_sleep = Some(t);
                // }
                start_sleep = Some(t);
            }
            Record::Wake(t) => {
                // if current_guard.unwrap() == best {
                //     let sleep_min = start_sleep.unwrap().minutes;
                //     let wake_min = t.minutes;
                //     for i in sleep_min..wake_min { counts[i as usize] += 1; }
                // }
                let counts =
                    counts_map.entry(current_guard.unwrap()).or_insert([0u32; 60]);
                let sleep_min = start_sleep.unwrap().minutes;
                let wake_min = t.minutes;
                for i in sleep_min..wake_min { counts[i as usize] += 1; }
            }
        }
    }
    // oh it's argmax not max
    let mut ind = 0;
    let mut high = &0;
    let counts = counts_map[best];
    for (i, c) in counts.iter().enumerate() {
        if c > &high {
            ind = i;
            high = &c;
        }
    }
    println!("part 1 = {}", *best * ind as u32);  // 71748
    part2(counts_map);
}

fn part2(counts_map: HashMap<&u32, [u32; 60]>) {
    let mut best_id = &0;
    let mut best_minute = 0;
    let mut best_count = 0;
    for (id, counts) in counts_map {
        for (i, c) in counts.into_iter().enumerate() {
            if c > best_count {
                best_id = id;
                best_minute = i;
                best_count = c;
            }
        }
    }
    println!("part 2 = {}", best_id * best_minute as u32);  // 106850
}
