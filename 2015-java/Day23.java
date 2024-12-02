import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day23 {
    public static void main(String[] args) {
        String path = "c:\\users\\xdavi\\documents\\temp\\input.txt";
        Computer comp = Computer.from(read(path));
        System.out.println("part 1 = " + comp.run());
        comp.reset(1);
        System.out.println("part 2 = " + comp.run());
    }
    static List<String> read(String path) {
        try (Scanner sc = new Scanner(new File(path))) {
            List<String> lns = new ArrayList<>();
            while (sc.hasNextLine()) lns.add(sc.nextLine());
            return lns;
        } catch (FileNotFoundException e) { return null; }
    }
}

class Computer {
    long [] regA = {0}, regB = {0};
    int pos = 0;
    List<Instruction> ins;
    long run() {
        while (ins.get(pos).perform(this));
        return regB[0];
    }
    void reset(int a) {
        regA[0] = a;
        pos = 0;
        regB[0] = 0;
    }
    boolean shift(int off) {
        int next = pos + off;
        if (next < 0 || next >= ins.size()) return false;
        pos = next;
        return true;
    }
    static Computer from(List<String> lns) {
        Computer comp = new Computer();
        comp.ins = new ArrayList<>();
        for (String ln : lns) {
            comp.ins.add(Instruction.from(ln, comp));
        }
        return comp;
    }
}

interface Instruction {
    boolean perform(Computer comp);  // false if impossible, true otherwise
    static final Pattern UNARY = Pattern.compile("(\\w+) (a|b)");
    static final Pattern JUMP = Pattern.compile("jmp ([+-]\\d+)");
    static final Pattern JUMP_IF = Pattern.compile("ji(e|o) (a|b), ([+-]\\d+)");
    static final Function<Long, Long>
      HALF = (a -> a / 2), TRIPLE = (a -> a * 3), INCREMENT = (a -> a + 1);
    static final Function<Long, Boolean> EVEN = (a -> 0 == a % 2), ONE = (a -> 1 == a);
    static void bail() {
        System.err.println("something's wrong, bailing early.");
        System.exit(1);
    }
    static Instruction from(String s, Computer comp) {
        Matcher mat;
        if ((mat = UNARY.matcher(s)).matches()) {
            boolean onA = "a".equals(mat.group(2));
            switch (mat.group(1)) {
                case "hlf": return new Unary(HALF, onA);
                case "tpl": return new Unary(TRIPLE, onA);
                case "inc": return new Unary(INCREMENT, onA);
            }
        } else if ((mat = JUMP.matcher(s)).matches()) {
            return new Jump(Integer.parseInt(mat.group(1)));
        } else if ((mat = JUMP_IF.matcher(s)).matches()) {
            int off = Integer.parseInt(mat.group(3));
            boolean onA = "a".equals(mat.group(2));
            switch (mat.group(1)) {
                case "e": return new JumpIf(off, EVEN, onA);
                case "o": return new JumpIf(off, ONE, onA);
            }
        }
        bail();
        return null;
    }   
}

class Unary implements Instruction {
    Function<Long, Long> func;
    boolean onA;
    Unary(Function<Long, Long> f, boolean oa) {
        func = f;
        onA = oa;
    }
    @Override public boolean perform(Computer comp) {
        long[] reg = onA ? comp.regA : comp.regB;
        reg[0] = func.apply(reg[0]);
        return comp.shift(1);
    }
}

class Jump implements Instruction {
    int offset;
    Jump(int o) {
        offset = o;
    }
    @Override public boolean perform(Computer comp) {
        return comp.shift(offset);
    }
}

class JumpIf extends Jump {
    boolean onA;
    Function <Long, Boolean> func;
    JumpIf(int off, Function <Long, Boolean> f, boolean oa) {
        super(off);
        func = f;
        onA = oa;
    }
    @Override public boolean perform(Computer comp) {
        long[] reg = onA ? comp.regA : comp.regB;
        if (func.apply(reg[0])) return super.perform(comp);
        else return comp.shift(1);
    }
}
