import Foundation

func run(_ prog: [Int], _ a0: Int) -> [Int] {
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
            a >>= combo(operand)
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
            let nextOut = combo(operand) % 8
            out.append(nextOut)
        case 6:
            b = a >> combo(operand)
        case 7:
            c = a >> combo(operand)
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
    let out = run(prog, a0)
    // https://stackoverflow.com/a/43774452/2990344
    let p1 = out.map{String($0)}.joined(separator: ",")
    print("part 1 =", p1)  // 3,1,4,3,1,7,1,6,3
}

func part2() -> Int {
    let prog = [2,4,1,2,7,5,4,5,1,3,5,5,0,3,3,0]
    
    // 67 -> [3,3,0]
    // 34665 -> [5,5,0,3,3,0]
    // 17748484 -> [5,1,3,5,5,0,3,3,0]
    // 9087224139 -> [7,5,4,5,1,3,5,5,0,3,3,0]
    // 37221270076916 -> [2,4,1,2,7,5,4,5,1,3,5,5,0,3,3,0]
    for x in 0..<1_000_000 {
        let a = (9087224139 << 12) + x
        if [2,4,1,2,7,5,4,5,1,3,5,5,0,3,3,0] == run(prog, a) {
            return a
        }
    }
    return 0
}
// can make the above programmatic by doing it chunks of 4 instead of manual
// because prog is 16 ints long.

print("part 2 =", part2())  // 37221270076916



/*

prog
 [0,3,5,4,3,0]
 117440
 
 0 3   A /= 8
 5 4   output A % 8
 3 0   if A != 0 { jump to 0 }
 
 in octal, it's the digits reversed
 345300
 
 extra zero because of extra A /= 8 in beginning
 
 >>> 0o345300
 117440
  
 [2,4,1,2,7,5,4,5,1,3,5,5,0,3,3,0]
 
 0, 1, 2, 3, 4, 5, 7
 
 2 -> B ^= 3 (011), 001 -> B = 1 before that
 (A >> B) ^ B = 1
 
 wait, you should optimize from the RIGHT. What is the smallest to output 0,
 then to add another octit to it to end in 0,3, then add another octit etcetc
 
 2 4   B = A % 8
 1 2   B ^= 2
 7 5   C = A >> B
 4 5   B ^= C
 1 3   B ^= 3
 5 5   output B % 8
 0 3   A = A / 8
 3 0   if A != 0 { jump to 0 }
 
 */
