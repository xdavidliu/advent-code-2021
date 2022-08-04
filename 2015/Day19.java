import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day19 {
    static String medicine;
    static Map<String, List<String>> table, inverseTable;
    public static void main(String[] args) {
        String path = "c:\\users\\xdavi\\documents\\temp\\input.txt";
        read(path);
        System.out.println("part 1 = " + solvePart1());
        solvePart2();
    }
    static int solvePart1() {
        Set<String> justSeen = new TreeSet<>();
        for (Map.Entry<String, List<String>> entry : table.entrySet()) {
            tryReplace(entry.getKey(), entry.getValue(), medicine, justSeen);
        }
        return justSeen.size();
    }
    static void solvePart2() {
        List<String> keys = new ArrayList<>(inverseTable.keySet());
        keys.sort((a, b) -> Integer.compare(b.length(), a.length()));
        recurse(medicine, keys, inverseTable, 0);
    }
    static void recurse(String txt, List<String> keys, Map<String, List<String>> inverseTable, int steps) {
        for (String k : keys) {
            if (k.length() > txt.length()) continue;
            Set<String> justSeen = new TreeSet<>();
            tryReplace(k, inverseTable.get(k), txt, justSeen);
            for (String seen : justSeen) {
                if (seen.equals("e")) {
                    System.out.println("part 2 = " + (steps+1));
                    System.exit(0);
                } else {
                    recurse(seen, keys, inverseTable, steps + 1);
                }
            }
        }
    }
    static void tryReplace(String key, List<String> vals, String txt, Set<String> justSeen) {
        for (int i = txt.indexOf(key); i != -1; i = txt.indexOf(key, i+1)) {
            String before = txt.substring(0, i), after = txt.substring(i + key.length());
            for (String rep : vals) justSeen.add(before + rep + after);
        }
    }
    static void addToTable(String key, String val, Map<String, List<String>> table) {
        List<String> vals = table.get(key);
        if (vals == null) {
            vals = new ArrayList<>();
            table.put(key, vals);
        }
        vals.add(val);
    }
    static void read(String path) {
        try (Scanner sc = new Scanner(new File(path))) {
            Pattern pat = Pattern.compile("(\\w+) => (\\w+)");
            table = new TreeMap<>();
            inverseTable = new TreeMap<>();
            while (sc.hasNextLine()) {
                String ln = sc.nextLine();
                if (ln.isEmpty()) continue;  // second-to-last line
                Matcher mat = pat.matcher(ln);
                if (mat.matches()) {
                    String lhs = mat.group(1), rhs = mat.group(2);
                    addToTable(lhs, rhs, table);
                    addToTable(rhs, lhs, inverseTable);
                } else {  // last line, non-empty
                    medicine = ln;
                }
            }
        } catch (FileNotFoundException e) {}
    }
}
