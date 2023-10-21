use std::fs::read_to_string;

fn candidates(before: &[usize; 4], instruction: &[usize; 4], after: &[usize; 4]) -> Vec<String> {
    let funcs = [
        "addr", "addi", "mulr", "muli", "banr", "bani", "borr", "bori", "setr", "seti", "gtir",
        "gtri", "gtrr", "eqir", "eqri", "eqrr",
    ];
    let mut out = Vec::new();
    for f in funcs {
        let mut reg = before.clone();
        perform(f, &mut reg, instruction[1], instruction[2], instruction[3]);
        if &reg == after {
            out.push(f.to_owned());
        }
    }
    out
}

fn to_four_array(slice: &[usize]) -> [usize; 4] {
    let mut arr = [0; 4];
    arr.copy_from_slice(slice);
    arr
}

fn split_args(pat: &str, comma_separated_positive_ints: &str) -> Vec<usize> {
    comma_separated_positive_ints
        .split(pat)
        .map(|word| word.parse().unwrap())
        .collect()
}

fn strip_prefix_and_last_char<'a>(s: &'a str, prefix: &str) -> &'a str {
    assert!(s.starts_with(prefix), "s = {}, prefix = {}", s, prefix);
    &s[prefix.len()..s.len() - 1]
}

fn collapse(funcs: &mut Vec<Vec<String>>) {
    let mut done = Vec::new();
    while done.len() < funcs.len() {
        for line in funcs.iter_mut() {
            if line.len() != 1 {
                line.retain(|x| !done.contains(x));
            }
            if line.len() == 1 && !done.contains(&line[0]) {
                done.push(line[0].clone());
            }
        }
    }
}

fn main() {
    let path = "/home/xdavidliu/Documents/temp/data.txt";
    let text = read_to_string(path).unwrap();
    let mut lines = text.lines();
    let mut next = lines.next();
    let mut count = 0;
    let mut funcs = vec![Vec::new(); 16];
    while next.is_some_and(|line| line.starts_with("Before:")) {
        let before = split_args(", ", strip_prefix_and_last_char(next.unwrap(), "Before: ["));
        let before = to_four_array(&before);
        let instructions = split_args(" ", lines.next().unwrap());
        let instructions = to_four_array(&instructions);
        let after = split_args(
            ", ",
            strip_prefix_and_last_char(lines.next().unwrap(), "After:  ["),
        );
        let after = to_four_array(&after);
        let cands = candidates(&before, &instructions, &after);
        if 3 <= cands.len() {
            count += 1;
        }
        if funcs[instructions[0]].is_empty() {
            funcs[instructions[0]] = cands;
        } else {
            funcs[instructions[0]].retain(|s| cands.contains(s));
        }
        lines.next(); // Skip empty line.
        next = lines.next(); // Get ready for next "Before:" line.
    }
    collapse(&mut funcs);
    println!("part 1 = {}", count); //  607
                                    // There are several empty lines between the two sections.
    while next.is_some_and(|line| line.is_empty()) {
        next = lines.next();
    }
    let mut reg = [0; 4];
    while next.is_some() {
        let instruction = split_args(" ", next.unwrap());
        perform(
            &funcs[instruction[0]][0],
            &mut reg,
            instruction[1],
            instruction[2],
            instruction[3],
        );
        next = lines.next();
    }
    println!("part 2 = {}", reg[0]);  // 577
}

fn perform(name: &str, reg: &mut [usize; 4], a: usize, b: usize, c: usize) {
    let second = if name.ends_with("r") { reg[b] } else { b };
    reg[c] = match &name[0..3] {
        "add" => reg[a] + second,
        "mul" => reg[a] * second,
        "ban" => reg[a] & second,
        "bor" => reg[a] | second,
        "gti" => (a > second) as usize,
        "gtr" => (reg[a] > second) as usize,
        "eqi" => (a == second) as usize,
        "eqr" => (reg[a] == second) as usize,
        "set" => {
            if name.ends_with("r") {
                reg[a]
            } else {
                a
            }
        }
        _ => panic!(),
    };
}
