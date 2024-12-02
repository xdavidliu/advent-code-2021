import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day15 {
    static final int LIMIT = 100;
    static final int CALORIE = 500;
    static boolean restrictCalorie;
    public static void main(String[] args) {
        String path = "c:\\users\\xdavi\\documents\\temp\\input.txt";
        restrictCalorie = false;
        System.out.println("part 1 = " + solve(read(path)));
        restrictCalorie = true;
        System.out.println("part 2 = " + solve(read(path)));
    }
    static long solve(List<int[]> rows) {
        List<Integer> counts = new ArrayList<>();
        long[] best = {0};
        loop(best, counts, rows);
        return best[0];
    }
    static long choiceScore(List<Integer> counts, List<int[]> rows) {
        if (restrictCalorie) {
            int cals = 0, last = rows.get(0).length - 1;
            for (int i = 0; i < counts.size(); ++i) {
                cals += counts.get(i) * rows.get(i)[last];
            }
            if (cals != CALORIE) return 0;
        }
        long score = 1L;
        int nExceptCalorie = -1 + rows.get(0).length;
        for (int i = 0; i < nExceptCalorie; ++i) {
            int propertyScore = 0;
            for (int k = 0; k < counts.size(); ++k) {
                propertyScore += counts.get(k) * rows.get(k)[i];
            }
            score *= Math.max(0, propertyScore);
        }
        return score;
    }
    static void loop(long[] best, List<Integer> counts, List<int[]> rows) {
        if (counts.size() == rows.size()) best[0] = Math.max(best[0], choiceScore(counts, rows));
        else {
            assert counts.size() < rows.size();
            int remain = LIMIT;
            for (int count : counts) remain -= count;
            for (int i = 0; i <= remain; ++i) {
                List<Integer> nextCounts = new ArrayList<>(counts);
                nextCounts.add(i);
                loop(best, nextCounts, rows);
            }
        }
    }
    static List<int[]> read(String path) {
        try (BufferedReader rd = new BufferedReader(new FileReader(path))) {
            String ln;
            List<int[]> rows = new ArrayList<>();
            String re = "\\w+: capacity (-?\\d+), durability (-?\\d+), " +
                        "flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)";
            Pattern pat = Pattern.compile(re);
            while (null != (ln = rd.readLine())) {
                int[] row = new int[5];
                Matcher mat = pat.matcher(ln);
                if (!mat.matches()) continue;
                for (int i = 0; i < 5; ++i) row[i] = Integer.parseInt(mat.group(i+1));
                rows.add(row);
            }
            return rows;
        } catch (IOException e) { return null; }
    }
}
