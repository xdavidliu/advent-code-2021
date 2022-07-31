import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Day12 {
    public static void main(String[] args) {
        String txt = getText("c:\\users\\xdavi\\documents\\temp\\input.txt");
        includeRed = true;
        System.out.println("part 1 = " + solve(txt));
        includeRed = false;
        System.out.println("part 2 = " + solve(txt));
    }
    static int solve(String txt) {
        if (txt == null) return -1;
        int[] score = {0};
        consume(txt, 0, score);
        return score[0];
    }
    static boolean includeRed;
    static final String DELIM = ",";
    static String getText(String path) {
        try (BufferedReader rd = new BufferedReader(new FileReader(path))) {
            return rd.readLine();
        } catch (IOException e) { return null; }
    }
    static int consumeName(String txt, int pos) {
        return 1 + txt.indexOf('"', pos + 1);
    }
    static int consumeInt(String txt, int pos, int[] score) {
        int i = pos;
        if ('-' == txt.charAt(pos)) ++i;
        while (i < txt.length() && Character.isDigit(txt.charAt(i))) ++i;
        score[0] += Integer.parseInt(txt.substring(pos, i));
        return i;
    }
    static int consumeList(String txt, int pos, int[] score) {
        // Assumes txt.charAt(pos) == '['.
        int i = pos + 1;
        while (']' != txt.charAt(i = consume(txt, i, score))) {
            i += DELIM.length();
        }
        return i + 1;
    }
    static int consumeDict(String txt, int pos, int[] score) {
        // Assumes txt.charAt(pos) == '{'.
        int[] tempScore = {0};
        boolean redSeen = false;
        do {
            ++pos;  // first iteration is { and rest is ,
            int j = 1 + consumeName(txt, pos);  // skip the :
            pos = consume(txt, j, tempScore);
            if (!includeRed && !redSeen) {
                redSeen = '"' == txt.charAt(j) && "\"red\"".equals(txt.substring(j, pos));
            }
        } while ('}' != txt.charAt(pos));
        if (includeRed || !redSeen) score[0] += tempScore[0];
        return 1 + pos;
    }
    static int consume(String txt, int pos, int[] score) {
        // Assumes 0 <= pos < txt.length().
        switch (txt.charAt(pos)) {
            case '"': return consumeName(txt, pos);
            case '[': return consumeList(txt, pos, score);
            case '{': return consumeDict(txt, pos, score);
            default:  return consumeInt(txt, pos, score);
        }
    }
}
