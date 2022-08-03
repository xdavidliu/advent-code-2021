import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;

public class Day18 {
    public static void main(String[] args) {
        String path = "c:\\users\\xdavi\\documents\\temp\\input.txt";
        boolean[][] cur = grid(read(path)), curOriginal = copy(cur);
        cornersAlwaysOn = false;
        System.out.println("part 1 = " + solve(cur));
        cornersAlwaysOn = true;
        turnOnCorners(curOriginal);
        System.out.println("part 2 = " + solve(curOriginal));
    }
    static boolean[][] copy(boolean[][] arr) {
        boolean[][] cp = new boolean[arr.length][];
        for (int i = 0; i < cp.length; ++i) {
            cp[i] = Arrays.copyOf(arr[i], arr[i].length);
        }
        return cp;
    }
    static boolean cornersAlwaysOn;
    static int solve(boolean[][] cur) {
        boolean[][] next = new boolean[nrow+2][ncol+2];
        for (int i = 0; i < 100; ++i) {
            step(cur, next);
            boolean[][] temp = cur; cur = next; next = temp;
        }
        return countAllGrid(cur);
    }
    static int countAllGrid(boolean[][] cur) {
        int c = 0;
        for (boolean[] row : cur) {
            // WRONG! Compiles but gives wrong answer!
            // https://stackoverflow.com/q/4617567/2990344
            // It compiles because Arrays.asList(row) gives an Object that is dynamically
            // a List<boolean[]> with a single element, and true gets autoboxed to Boolean.TRUE,
            // and frequency treats the two as raw Objects which it calls .equals on and
            // obviously returns false.
            // c += Collections.frequency(Arrays.asList(row), true);
            // Simple way that works.
            for (boolean b : row) if (b) ++c;
        }
        return c;
    }
    static int countOnNeighbor(boolean[][] cur, int i, int k) {
        int[] diff = {-1, 0, 1};
        int count = 0;
        for (int a : diff) {
            for (int b : diff) {
                if (a == 0 && b == 0) continue;
                else if (cur[i+a][k+b]) ++count;
            }
        }
        return count;
    }
    static boolean isCorner(int i, int k) {
        return i == 1 && k == 1 || i == nrow && k == 1 ||
               i == 1 && k == ncol || i == nrow && k == ncol;
    }
    static void turnOnCorners(boolean[][] cur) {
        cur[1][1] = cur[1][ncol] = cur[nrow][1] = cur[nrow][ncol] = true;
    }
    static void step(boolean[][] cur, boolean[][] next) {
        for (int i = 1; i <= nrow; ++i) {
            for (int k = 1; k <= ncol; ++k) {
                int count = countOnNeighbor(cur, i, k);
                if (cornersAlwaysOn && isCorner(i, k)) next[i][k] = true;
                else if (cur[i][k]) next[i][k] = count == 2 || count == 3;
                else next[i][k] = count == 3;
            }
        }
    }
    static int nrow, ncol;
    static boolean[][] grid(List<String> lns) {
        nrow = lns.size();
        ncol = lns.get(0).length();
        boolean[][] res = new boolean[nrow+2][ncol+2];  // extra layer of sentinels
        for (int i = 0; i < nrow; ++i) {
            String ln = lns.get(i);
            for (int k = 0; k < ncol; ++k) res[i+1][k+1] = '#' == ln.charAt(k);
        }
        return res;
    }
    static List<String> read(String path) {
        try (Scanner sc = new Scanner(new File(path))) {
            List<String> res = new ArrayList<>();
            while (sc.hasNextLine()) res.add(sc.nextLine());
            return res;
        } catch (FileNotFoundException e) { return null; }
    } 
}
