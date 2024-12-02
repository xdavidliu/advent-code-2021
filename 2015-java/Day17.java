import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Scanner;

public class Day17 {
    public static void main(String[] args) {
        String path = "c:\\users\\xdavi\\documents\\temp\\input.txt";
        try (Scanner sc = new Scanner(new File(path))) {
            List<Integer> conts = new ArrayList<>();
            while (sc.hasNextInt()) conts.add(sc.nextInt());
            solve(conts, 150);
        } catch (FileNotFoundException e) {}
    }
    static void solve(List<Integer> conts, int high) {
        List<List<Integer>> ways = new ArrayList<>();
        for (int i = 0; i < 1 + high; ++i) ways.add(new ArrayList<Integer>());
        ways.get(0).add(0);
        for (int c : conts) {
            for (int x = high; x >= c; --x) {
                List<Integer> cur = ways.get(x);
                for (int q : ways.get(x-c)) cur.add(q + 1);
            }
        }
        List<Integer> highWays = ways.get(high);
        System.out.println("part 1 = " + highWays.size());
        int best = Collections.min(highWays);
        int count = Collections.frequency(highWays, best);
        System.out.println("part 2 = " + count);
    }
}
