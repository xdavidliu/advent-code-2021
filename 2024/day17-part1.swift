import Foundation

func run(_ prog: [Int], _ a0: Int, target: Optional<[Int]>) -> [Int] {
    var a = a0
    var b = 0
    var c = 0
    var ptr = 0
    func combo(_ i: Int) -> Int {
        return switch i {
        case 0, 1, 2, 3:
            i
        case 4:
            a
        case 5:
            b
        case 6:
            c
        default:
            fatalError("undefined combo arg")
        }
    }
    var out: [Int] = []
    while 0 <= ptr && ptr < prog.count {
        let opcode = prog[ptr]
        let operand = prog[ptr+1]
        switch opcode {
        case 0:
            a /= (1 << combo(operand))
        case 1:
            b ^= operand
        case 2:
            b = combo(operand) % 8
        case 3:
            if a != 0 {
                ptr = operand
                continue
            }
        case 4:
            b ^= c
        case 5:
            out.append(combo(operand) % 8)
            if let t = target {
                if out.last != t[out.count - 1] {
                    return []
                }
            }
        case 6:
            b = a / (1 << combo(operand))
        case 7:
            c = a / (1 << combo(operand))
        default:
            fatalError("unknown opcode")
        }
        ptr += 2
    }
    return out
}

func part1() {
    let prog = [2,4,1,2,7,5,4,5,1,3,5,5,0,3,3,0]
    let a0 = 64751475
    let out = run(prog, a0, target: nil)
    // https://stackoverflow.com/a/43774452/2990344
    let p1 = out.map{String($0)}.joined(separator: ",")
    print("part 1 =", p1)  // 3,1,4,3,1,7,1,6,3
}

//part1()

func part2() {
//    let prog = [0,3,5,4,3,0]
    let prog = [2,4,1,2,7,5,4,5,1,3,5,5,0,3,3,0]
    // have target when running that's Optional and end early as
    // soon as no match is found, to save time and short circuit.
    for a0 in 0...Int.max {
        if a0 % 10_000_000 == 0 {
            print(a0)
        }
        if prog == run(prog, a0, target: prog) {
            print(a0)
            return
        }
    }
}

part2()
