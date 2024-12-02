import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day16 {
    public static void main(String[] args) {
        String path = "c:\\users\\xdavi\\documents\\temp\\input.txt";
        for (String ln : read(path)) {
            check(ln, 1);
            check(ln, 2);
        }
    }
    static final List<String> GREATER = List.of("cats", "trees");
    static final List<String> LESS = List.of("pomeranians", "goldfish");
    static void check(String ln, int part) {
        Matcher mat = pat.matcher(ln);
        if (!mat.matches()) return;
        int sue = Integer.parseInt(mat.group(1));
        for (int i = 2; i < 7; i += 2) {
            String key = mat.group(i);
            int known = Integer.parseInt(mat.group(i+1));
            int val = tapeOutput.get(key);
            if (part == 2 && GREATER.contains(key)) {
                if (known <= val) return;
            } else if (part == 2 && LESS.contains(key)) {
                if (known >= val) return;
            } else if (known != val) return;
        }
        System.out.println("part " + part + " = " + sue);
    }
    static Pattern pat = Pattern.compile("Sue (\\d+): (\\w+): (\\d+), (\\w+): (\\d+), (\\w+): (\\d+)");
    static List<String> read(String path) {
        try (BufferedReader rd = new BufferedReader(new FileReader(path))) {
            List<String> ls = new ArrayList<>();
            String ln;
            while (null != (ln = rd.readLine())) ls.add(ln);
            return ls;
        } catch (IOException e) { return null; }
    }
    static Map<String, Integer> tapeOutput =
      Map.of("children", 3, "cats", 7, "samoyeds", 2, "pomeranians", 3, "akitas", 0,
             "vizslas", 0, "goldfish", 5, "trees", 3, "cars", 2, "perfumes", 1);
}
