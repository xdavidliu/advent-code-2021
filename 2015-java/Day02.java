import java.nio.file.*; import java.util.Arrays;
public class Day02 {
    static void solve() throws java.io.IOException {
        int p1 = 0, p2 = 0; for (var s : Files.readAllLines(Path.of("./input/day02.txt"))) {
            var a = Arrays.stream(s.split("x")).mapToInt(Integer::parseInt).toArray();
            Arrays.sort(a); int prod = a[0] * a[1], twoSum = 2 * (a[0] + a[1]);
            p1 += 3 * prod + a[2] * twoSum; p2 += twoSum + prod * a[2];
        } System.out.printf("02 %d %d\n", p1, p2);
    }
}