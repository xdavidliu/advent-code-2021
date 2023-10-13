fn main() {
    let input_str = "540561";
    let mut scores: Vec<u8> = vec![3, 7];
    let input: usize = input_str.parse().unwrap();
    let input_vec: Vec<_> = input_str
        .chars()
        .map(|c| c.to_digit(10).unwrap() as u8)
        .collect();
    let (mut a, mut b) = (0, 1);
    let (mut part1_done, mut part2_done) = (false, false);
    while !(part1_done && part2_done) {
        let (p, q) = combine(scores[a], scores[b]);
        if let Some(d) = p {
            scores.push(d);
            // copied wholesale; not sure how to abstract out
            if !part2_done && scores.ends_with(&input_vec) {
                println!("part 2 = {}", scores.len() - input_vec.len());  // 20254833
                part2_done = true;
            }
        }
        scores.push(q);
        a = (a + 1 + scores[a] as usize) % scores.len();
        b = (b + 1 + scores[b] as usize) % scores.len();
        if !part1_done && scores.len() >= input + 10 {
            print!("part 1 = ");  // 1413131339
            for s in &scores[input..input + 10] {
                print!("{}", s);
            }
            println!();
            part1_done = true;
        }
        if !part2_done && scores.ends_with(&input_vec) {
            println!("part 2 = {}", scores.len() - input_vec.len());
            part2_done = true;
        }
    }
}

fn combine(x: u8, y: u8) -> (Option<u8>, u8) {
    let s = x + y;
    let p = if s >= 10 { Some(s / 10) } else { None };
    (p, s % 10)
}
