import java.nio.file.*;
import java.security.*;
public class Day04Fast {
    static int increment(byte[] bs, int left, int right) {
        byte carry = 1;
        for (int i = right-1; carry == 1 && i >= left; --i) {
            if (bs[i] == '9') { bs[i] = '0'; } else { ++bs[i]; carry = 0; }
        }
        if (carry == 1) {
            for (int i = right; i > left; --i) { bs[i] = bs[i-1]; }
            bs[left] = '1'; return right + 1;
        } else { return right; }
    }
    static void solve() throws java.io.IOException, NoSuchAlgorithmException, DigestException {
        var md = MessageDigest.getInstance("MD5");
        var cs = Files.readString(Path.of("./input/day04.txt")).getBytes();
        int p1 = -1, p2 = -1, left = cs.length, right = left + 1;
        var sbs = new byte[1000];
        System.arraycopy(cs, 0, sbs, 0, cs.length);
        sbs[right - 1] = '0';
        var bs = new byte[16];
        for (int i = 0; p1 == -1 || p2 == -1; ++i) {
            md.update(sbs, 0, right); md.digest(bs, 0, bs.length);
            if (bs[0] == 0 && bs[1] == 0) {
                if (p1 == -1 && 0 == (bs[2] & 0xF0)) { p1 = i; }
                if (p2 == -1 && 0 == bs[2]) { p2 = i; }
            }
            right = increment(sbs, left, right);
        } System.out.printf("04 %d %d\n", p1, p2);
    }
}