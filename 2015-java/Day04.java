import java.nio.file.*; import java.security.*;
public class Day04 {
    static void solve() throws java.io.IOException, NoSuchAlgorithmException {
        var s = Files.readString(Path.of("./input/day04.txt")).stripTrailing();
        int p1 = -1, p2 = -1; var md = MessageDigest.getInstance("MD5");
        for (int i = 0; p1 == -1 || p2 == -1; ++i) {
            var bs = md.digest((s+i).getBytes()); if (bs[0] == 0 && bs[1] == 0) {
                if (p1 == -1 && 0 == (bs[2] & 0xF0)) { p1 = i; }
                if (p2 == -1 && 0 == bs[2]) { p2 = i; }
            }
        } System.out.printf("04 %d %d\n", p1, p2);
    }
}