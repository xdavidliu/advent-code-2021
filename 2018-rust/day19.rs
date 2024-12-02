use std::fs::read_to_string;

fn main() {
    let path = "/home/xdavidliu/Documents/temp/data.txt";
    let text = read_to_string(path).unwrap();
    let mut lines = text.lines();
    let line = lines.next().unwrap();
    // begin pointer
    assert!(line.starts_with("#ip"));
    let mut words = line.split(" ");
    words.next(); // consume "#ip"
    let ip: usize = words.next().unwrap().parse().unwrap();
    // end getting pointer
    let mut instructions = Vec::new();
    while let Some(line) = lines.next() {
        let mut split = line.split(" ");
        let name = split.next().unwrap().to_owned();
        let a: usize = split.next().unwrap().parse().unwrap();
        let b: usize = split.next().unwrap().parse().unwrap();
        let c: usize = split.next().unwrap().parse().unwrap();
        instructions.push(Instruction { name, a, b, c });
    }
    let mut reg1 = vec![0; 6];
    reg1 = vec![0, 860, 11, 1, 4, 11];
    run(&mut reg1, &instructions, ip);
    println!("part 1 = {}", reg1[0]);  // 1848

    // println!("part 2 = {}", reg2[0]);  // 10551261 too low 22157688
    // println!("reg[ip] = {}", reg2[ip]);

    // let mut reg2 = vec![10551261, 10551260, q, 10551260, 4, q * 10551260];
    // run(&mut reg2, &instructions, ip);
    /*
    phase transition
    0, 10551260, 10551260, 1, 4, 10551260
    0, 10551260, 10551260, 1, 5, 1
    0, 10551260, 10551260, 1, 7, 1
    1, 10551260, 10551260, 1, 8, 1
    1, 10551260, 10551261, 1, 9, 1

    ...

    1, 10551260, 5275630, 2, 4, 10551260   <-----------
    1, 10551260, 5275630, 2, 5, 1
    1, 10551260, 5275630, 2, 7, 1      "addr 3 0 0"
    3, 10551260, 5275630, 2, 8, 1
    3, 10551260, 5275631, 2, 9, 1
    3, 10551260, 5275631, 2, 10, 0
    3, 10551260, 5275631, 2, 11, 0
    3, 10551260, 5275631, 2, 3, 0
    3, 10551260, 5275631, 2, 4, 10551262

    look at last element there. 10551260 is divisible by 2
    so the addr instruction adds it to reg[0]
    so looks like reg[0] is sum of all integers divisible by 10551260
     */
    let mut ans2 = 0;
    let mut k = 1;
    let n = 10551260;  // see notes below
    while k * k < n {  // assume n not perfect square
        if n % k == 0 {
            ans2 += k + n / k;
        }
        k += 1;
    }
    println!("part 2 = {}", ans2);
}

fn run(reg: &mut Vec<usize>, instructions: &Vec<Instruction>, ip: usize) {
    while (0..instructions.len()).contains(&reg[ip]) {
        instructions[reg[ip]].perform(reg);
        reg[ip] += 1;
        // println!("{}, {}, {}, {}, {}, {}", reg[0], reg[1], reg[2], reg[3], reg[4], reg[5]);
    }
}

struct Instruction {
    name: String,
    a: usize,
    b: usize,
    c: usize,
}

impl Instruction {
    fn perform(&self, reg: &mut [usize]) {
        perform(&self.name, reg, self.a, self.b, self.c);
    }
}

// copied from day 16, without ; 4 in reg because now 6
fn perform(name: &str, reg: &mut [usize], a: usize, b: usize, c: usize) {
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
