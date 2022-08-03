import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day13 {

    public static void main(String[] args) {
        int[][] mat = Build.collect("c:\\users\\xdavi\\documents\\temp\\input13.txt");
        System.out.println("part 1 = " + solve(mat, true));
        System.out.println("part 2 = " + solve(mat, false));
    }
    static int solve(int[][] mat, boolean round) {
        int n = mat.length;
        assert n == mat[0].length;
        int[] seq = new int[round ? n-1 : n];
        for (int i = 0; i < seq.length; ++i) seq[i] = i;
        int best = Integer.MIN_VALUE;
        do {
            best = Math.max(best, score(mat, seq));
        } while (Permute.permute(seq));
        return best;
    }
    static int score(int[][] mat, int[] seq) {
        int n = mat.length;
        int sum = 0;
        for (int i = 0; i < seq.length-1; ++i) {
            sum += mat[seq[i]][seq[i+1]];
            sum += mat[seq[i+1]][seq[i]];
        }
        if (n != seq.length) {
            assert n == 1 + seq.length;
            sum += mat[seq[0]][n-1];
            sum += mat[n-1][seq[0]];
            sum += mat[seq[n-2]][n-1];
            sum += mat[n-1][seq[n-2]];
        }
        return sum;
    }
}

class Build {
    static List<String> names = new ArrayList<>();
    static int getIndex(String name) {
        int i = names.indexOf(name);
        if (i != -1) return i;
        names.add(name);
        return names.size() - 1;
    }
    static final String reg = "(\\w+) would (gain|lose) (\\d+) happiness units by sitting next to (\\w+).";
    static final Pattern pat = Pattern.compile(reg);
    static int[] element(String ln) {
        Matcher mat;
        if (!(mat = pat.matcher(ln)).matches()) return null;
        int i = getIndex(mat.group(1));
        int j = getIndex(mat.group(4));
        int sign = "gain".equals(mat.group(2)) ? 1 : -1;
        int change = sign * Integer.parseInt(mat.group(3));
        return new int[] {i, j, change};
    }
    static List<String> readLines(String path) {
        List<String> lns = new ArrayList<>();
        try (BufferedReader rd = new BufferedReader(new FileReader(path))) {
            String ln;
            while (null != (ln = rd.readLine())) lns.add(ln);
        } catch (IOException e) {}
        return lns;
    }
    static int[][] collect(String path) {
        List<int[]> elems = new ArrayList<>();
        for (String ln : readLines(path)) {
            int[] elem = element(ln);
            if (elem != null) elems.add(elem);
        }
        int n = names.size();
        int[][] mat = new int[n][n];
        for (int[] elem : elems) mat[elem[0]][elem[1]] = elem[2];
        return mat;
    }
}
