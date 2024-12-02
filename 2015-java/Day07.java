import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class Operand {
    private String str;
    private Integer val;
    Operand(String s) {
        try { val = Integer.parseInt(s); }
        catch (NumberFormatException e) { str = s; }
    }
    Operand(Integer x) { val = x; }
    Integer value(Map<String, Integer> table) {
        if (val == null) val = table.get(str);
        return val;
    }
}

interface Instruction {
    boolean perform(Map<String, Integer> table);
    static Pattern pAssign = Pattern.compile("(\\w+) -> ([a-z]+)");
    static Pattern pNegate = Pattern.compile("NOT (\\w+) -> ([a-z]+)");
    static Pattern pAnd = Pattern.compile("(\\w+) AND (\\w+) -> ([a-z]+)");
    static Pattern pOr = Pattern.compile("(\\w+) OR (\\w+) -> ([a-z]+)");
    static Pattern pLShift = Pattern.compile("(\\w+) LSHIFT (\\w+) -> ([a-z]+)");
    static Pattern pRShift = Pattern.compile("(\\w+) RSHIFT (\\w+) -> ([a-z]+)");
    static Instruction create(String ln) {
        Matcher mc;
        if ((mc = pAssign.matcher(ln)).matches()) {
            return new Assign(new Operand(mc.group(1)), mc.group(2));
        } else if ((mc = pNegate.matcher(ln)).matches()) {
            return new Negate(new Operand(mc.group(1)), mc.group(2));
        } else if ((mc = pAnd.matcher(ln)).matches()) {
            return new And(new Operand(mc.group(1)),
                           new Operand(mc.group(2)),
                           mc.group(3));
        } else if ((mc = pOr.matcher(ln)).matches()) {
            return new Or(new Operand(mc.group(1)),
                          new Operand(mc.group(2)),
                          mc.group(3));
       } else if ((mc = pLShift.matcher(ln)).matches()) {
            return new LShift(new Operand(mc.group(1)),
                              new Operand(mc.group(2)),
                              mc.group(3));
       } else if ((mc = pRShift.matcher(ln)).matches()) {
            return new RShift(new Operand(mc.group(1)),
                              new Operand(mc.group(2)),
                              mc.group(3));
       } else {
            System.err.println("did not match any regexes: " + ln);
            return null;
       }     
    }
    boolean assignsB();
}

abstract class UnaryInstruction implements Instruction {
    private Operand oper;
    private String rhs;
    UnaryInstruction(Operand oper, String rhs) {
        this.oper = oper;
        this.rhs = rhs;
    }
    @Override public boolean perform(Map<String, Integer> table) {
        Integer val;
        if (null == (val = oper.value(table))) return false;
        table.put(rhs, function(val));
        return true;
    }
    public boolean assignsB() { return rhs.equals("b"); }
    abstract Integer function(Integer x);
}

class Assign extends UnaryInstruction {
    Assign(Operand oper, String rhs) { super(oper, rhs); }
    @Override Integer function(Integer x) { return x; }
}

class Negate extends UnaryInstruction {
    Negate(Operand oper, String rhs) { super(oper, rhs); }
    @Override Integer function(Integer x) { return ~x; }
}

abstract class BinaryInstruction implements Instruction {
    private Operand o1, o2;
    private String rhs;
    BinaryInstruction(Operand o1, Operand o2, String rhs) {
        this.o1 = o1;
        this.o2 = o2;
        this.rhs = rhs;
    }
    @Override public boolean perform(Map<String, Integer> table) {
        Integer v1, v2;
        if (null == (v1 = o1.value(table))) return false;
        if (null == (v2 = o2.value(table))) return false;
        table.put(rhs, function(v1, v2));
        return true;
    }
    public boolean assignsB() { return rhs.equals("b"); }
    abstract Integer function(Integer x, Integer y);
}

class And extends BinaryInstruction {
    And(Operand o1, Operand o2, String rhs) { super(o1, o2, rhs); }
    Integer function(Integer x, Integer y) { return x & y; }
}

class Or extends BinaryInstruction {
    Or(Operand o1, Operand o2, String rhs) { super(o1, o2, rhs); }
    Integer function(Integer x, Integer y) { return x | y; }
}

class LShift extends BinaryInstruction {
    LShift(Operand o1, Operand o2, String rhs) { super(o1, o2, rhs); }
    Integer function(Integer x, Integer y) { return x << y; }
}

class RShift extends BinaryInstruction {
    RShift(Operand o1, Operand o2, String rhs) { super(o1, o2, rhs); }
    Integer function(Integer x, Integer y) { return x >> y; }
}

public class Day07 {
    static Integer solve(List<Instruction> ins) {
        Map<String, Integer> table = new TreeMap<>();
        Iterator<Instruction> iter = ins.iterator();
        while (!ins.isEmpty() && !table.containsKey("a")) {
            if (!iter.hasNext()) iter = ins.iterator();
            if (iter.next().perform(table)) iter.remove();
        }
        return table.get("a");
    }
    static void preparePart2(Integer part1, ArrayList<Instruction> ins) {
        ins.removeIf(x -> x.assignsB());
        Instruction newAssignB = new Assign(new Operand(part1), "b");
        ins.add(newAssignB);
    }
    public static void main(String[] args) {
        // Create two copies and populate them independently, since the Operands have
        // state which changes as you solve.
        ArrayList<Instruction> insPart1 = new ArrayList<>();
        ArrayList<Instruction> insPart2 = new ArrayList<>();
        String path = "C:\\Users\\xdavi\\Documents\\temp\\input.txt";
        try (BufferedReader rd = new BufferedReader(new FileReader(path))) {
            String ln;
            while (null != (ln = rd.readLine())) {
                insPart1.add(Instruction.create(ln));
                insPart2.add(Instruction.create(ln));
            }
        } catch (IOException e) {}
        if (!insPart1.isEmpty()) {
            Integer answerPart1 = solve(insPart1);
            preparePart2(answerPart1, insPart2);
            System.out.println("part 1 = " + answerPart1);
            System.out.println("part 2 = " + solve(insPart2));
        }
    }
}
