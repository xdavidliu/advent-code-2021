import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day09 {
    static Integer[][] matrix(List<int[]> edges, int n) {
        // Integer (not int) so that default is null, not 0.
        Integer[][] mat = new Integer[n][n];
        for (int[] e : edges) {
            mat[e[0]][e[1]] = e[2];
            mat[e[1]][e[0]] = e[2];
        }
        return mat;
    }
    static List<String> readLines(String path) {
        try (BufferedReader rd = new BufferedReader(new FileReader(path))) {
            List<String> lines = new ArrayList<>();
            String ln;
            while (null != (ln = rd.readLine())) {
                lines.add(ln);
            }
            return lines;
        } catch (IOException e) { return null; }
    }
    static Integer[][] parseMatrix(List<String> lines) {
            Pattern pat = Pattern.compile("(\\w+) to (\\w+) = (\\d+)");
            Matcher mc;
            List<String> places = new ArrayList<>();
            List<int[]> edges = new ArrayList<>();
            for (String ln : lines) {
                if ((mc = pat.matcher(ln)).matches()) {
                    String a = mc.group(1), b = mc.group(2);
                    int ia = places.indexOf(a), ib = places.indexOf(b);
                    if (ia == -1) {
                        places.add(a);
                        ia = places.size() - 1;
                    }
                    if (ib == -1) {
                        places.add(b);
                        ib = places.size() - 1;
                    }
                    edges.add(new int[]{ia, ib, Integer.parseInt(mc.group(3))});
                }
            }
            return matrix(edges, places.size());
    }
    static void swap(int[] seq, int i, int k) {
        int temp = seq[i];
        seq[i] = seq[k];
        seq[k] = temp;
    }
    static void reverseFrom(int[] seq, int i) {
        for (int k = seq.length - 1; i < k; ++i, --k) {
            swap(seq, i, k);
        }
    }
    static boolean permute(int[] seq) {
        // Assumes distinct elements.
        int i = seq.length - 2;
        while (i >=0 && seq[i] > seq[i+1]) --i;
        if (i < 0) return false;
        int k = seq.length - 1;
        while (seq[i] > seq[k]) --k;
        swap(seq, i, k);
        reverseFrom(seq, i+1);
        return true;
    }
    static Integer score(int[] seq, Integer[][] mat) {
        Integer sum = 0;
        for (int i = 0; i < seq.length - 1; ++i) {
            Integer dist = mat[seq[i]][seq[i+1]];
            if (dist == null) return null;
            sum += dist;
        }
        return sum;
    }
    public static void main(String[] args) {
        String path = "c:\\users\\xdavi\\documents\\temp\\input.txt";
        Integer[][] mat = parseMatrix(readLines(path));
        int[] seq = new int[mat.length];
        for (int i = 0; i < seq.length; ++i) seq[i] = i;
        Integer low = score(seq, mat), high = low;
        while (permute(seq)) {
            Integer sc = score(seq, mat);
            if (sc == null) continue;
            if (low == null || sc < low) low = sc;
            if (high == null || sc > high) high = sc;
        }
        System.out.println("part 1 = " + low);
        System.out.println("part 2 = " + high);
    }
}
