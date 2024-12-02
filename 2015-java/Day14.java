import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Day14 {
    static class Deer {
        private int speed, run, rest, time, dist, score;
        Deer(int[] stats) {
            speed = stats[0]; run = stats[1]; rest = stats[2];
            time = 0; dist = 0; score = 0;
        }
        void increment() {
            ++time;
            int rem = time % (run + rest);
            if (0 < rem && rem <= run) dist += speed;
        }
        void givePoint() { ++score; }
        int getDistance() { return dist; }
        int getScore() { return score; }
    }
    static List<Deer> collect(String path) {
        List<Deer> rows = new ArrayList<>();
        try (BufferedReader rd = new BufferedReader(new FileReader(path))) {
            String reg = "\\w+ can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds.";
            Pattern pat = Pattern.compile(reg);
            for (String ln; null != (ln = rd.readLine());) {
                Matcher mat;
                if (!(mat = pat.matcher(ln)).matches()) continue;
                int[] row = new int[3];
                for (int i = 0; i < 3; ++i) row[i] = Integer.parseInt(mat.group(i+1));
                rows.add(new Deer(row));
            }
        } catch (IOException e) {}
        return rows;
    }
    public static void main(String[] args) {
        String path = "c:\\users\\xdavi\\documents\\temp\\input.txt";
        int totalTime = 2503;
        List<Deer> ds = collect(path);
        for (int i = 0; i < totalTime; ++i) {
            int best = Integer.MIN_VALUE;
            for (Deer d : ds) {
                d.increment();
                best = Math.max(best, d.getDistance());
            }
            for (Deer d : ds) {
                if (best == d.getDistance()) d.givePoint();
            }
            if (i+1 == totalTime) System.out.println("part 1 = " + best);
        }
        int bestScore = Integer.MIN_VALUE;
        for (Deer d : ds) bestScore = Math.max(bestScore, d.getScore());
        System.out.println("part 2 = " + bestScore);
    }
}
